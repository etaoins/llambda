#include "SharedByteArray.h"

#include "platform/memory.h"

#include <cstring>
#include <cassert>
#include <new>

namespace lliby
{

namespace
{
#ifdef _LLIBY_CHECK_LEAKS
	std::atomic<std::size_t> allocationCount(0);
#endif

	std::size_t objectSizeForBytes(std::size_t bytes)
	{
		return sizeof(SharedByteArray) + bytes;
	}

	std::size_t bytesForObjectSize(std::size_t objectSize)
	{
		return objectSize - sizeof(SharedByteArray);
	}
}


SharedByteArray::SharedByteArray(RefCountType initialRefCount) :
	m_refCount(initialRefCount),
	m_cachedHashValue(SharedByteHash::ImpossibleResultValue)
{
	incrementInstanceCount();
}

SharedByteArray* SharedByteArray::createInstance(std::size_t bytes)
{
	// This is tricky because we're a variable length object
	// Calculate our required length and then place the object
	void *allocPlacement = malloc(objectSizeForBytes(bytes));

	return new (allocPlacement) SharedByteArray(1);
}

std::size_t SharedByteArray::capacity(std::size_t fallbackCapacity)
{
	return bytesForObjectSize(platform::mallocActualSize(this, objectSizeForBytes(fallbackCapacity)));
}

SharedByteArray* SharedByteArray::destructivelyResizeTo(std::size_t bytes)
{
	assert(isExclusive() && !isSharedConstant());

	const std::size_t allocSize = objectSizeForBytes(bytes);
	return reinterpret_cast<SharedByteArray*>(realloc(this, allocSize));
}

SharedByteArray* SharedByteArray::asWritable(std::size_t bytes)
{
	if (isExclusive())
	{
		// We have an exclusive copy. Make sure we invalidate our hash value before modification.
		m_cachedHashValue = SharedByteHash::ImpossibleResultValue;
		return this;
	}
	else
	{
		// We need to fork this
		SharedByteArray *duplicateCopy = createInstance(bytes);
		memcpy(duplicateCopy->m_data, m_data, bytes);

		// Unref ourselves
		unref();

		return duplicateCopy;
	}
}

SharedByteArray::HashValueType SharedByteArray::hashValue(std::size_t size) const
{
	if (m_cachedHashValue == SharedByteHash::ImpossibleResultValue)
	{
		SharedByteHash byteHasher;
		m_cachedHashValue = byteHasher(m_data, size);
	}

	return m_cachedHashValue;
}

bool SharedByteArray::isEqual(const SharedByteArray *other, std::size_t size) const
{
	if (this == other)
	{
		return true;
	}

	if ((m_cachedHashValue != SharedByteHash::ImpossibleResultValue) &&
			(other->m_cachedHashValue != SharedByteHash::ImpossibleResultValue))
	{
		if (m_cachedHashValue != other->m_cachedHashValue)
		{
			return false;
		}
	}

	return memcmp(m_data, other->m_data, size) == 0;
}

bool SharedByteArray::unref()
{
	if (isSharedConstant())
	{
		// Don't decrement; we're readonly
		return false;
	}

	const bool shouldDestroy = m_refCount.fetch_sub(1u, std::memory_order_release) == 1;

	if (shouldDestroy)
	{
		// We were the last reference
		// Make sure the memory operations from this delete are strictly after the fetch_sub
		std::atomic_thread_fence(std::memory_order_acquire);

		delete this;
		return true;
	}

	return false;
}

#ifdef _LLIBY_CHECK_LEAKS

SharedByteArray::~SharedByteArray()
{
	allocationCount.fetch_sub(1, std::memory_order_relaxed);
}

void SharedByteArray::incrementInstanceCount()
{
	allocationCount.fetch_add(1, std::memory_order_relaxed);
}

std::size_t SharedByteArray::instanceCount()
{
	return allocationCount.load(std::memory_order_relaxed);
}

#else

void SharedByteArray::incrementInstanceCount()
{
}

std::size_t SharedByteArray::instanceCount()
{
	return 0;
}

#endif

}

