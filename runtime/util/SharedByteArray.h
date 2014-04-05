#ifndef _LLIBY_UTIL_SHAREDBYTEARRAY_H
#define _LLIBY_UTIL_SHAREDBYTEARRAY_H

#include <cstdlib>
#include <cstdint>
#include <cstring>
#include <cassert>

#include <limits>
#include <atomic>
#include <new>

#include "platform/memory.h"

namespace lliby
{

/**
 * Thread-safe copy-on-write array of bytes
 *
 * The size of the byte array must be stored externally to the byte array itself. This reflects the fact that the 
 * various cell types include the actual data length in their cells.
 */
class SharedByteArray
{
public:
	/**
	 * Creates a new SharedByteArray instance of the exactly the given size
	 */
	static SharedByteArray* createInstance(size_t bytes)
	{
		// This is tricky because we're a variable length object
		// Calculate our required length and then place the object
		const size_t allocSize = sizeof(m_refCount) + bytes; 
		void *allocPlacement = malloc(allocSize);

		return new (allocPlacement) SharedByteArray(1);
	}

	/**
	 * Creates a new SharedByteArray instance of at least the given size
	 *
	 * This uses lliby::sizedMalloc() internally to determine how much slack allocation was provided by the platform's
	 * malloc() implementation
	 *
	 * @param  bytes  Minimum size in types. On a successful return this will be the usable size in bytes of the byte 
	 *                array.
	 */
	static SharedByteArray *createMinimumSizedInstance(size_t &bytes)
	{
		size_t minimumAllocSize = sizeof(m_refCount) + bytes; 
		platform::SizedMallocResult sizedResult = platform::sizedMalloc(minimumAllocSize);

		// Update the actual size for the caller
		bytes = sizedResult.actualSize - sizeof(m_refCount);
		
		return new (sizedResult.basePointer) SharedByteArray(1);
	}

	/**
	 * Returns true if the caller has an exclusive copy of the byte array
	 *
	 * Exclusive byte array instances are by definition not a shared constan
	 */
	bool isExclusive() const
	{
		// If nobody else owns us then the current thread is the only one that can increment the refcount. This means
		// we need no atomicity dance here
		return m_refCount.load(std::memory_order_relaxed) == 1;
	}

	/**
	 * Returns true is this instance is a read-only shared constant
	 *
	 * These are only created by the codegen and may be placed in the rodata section. Any writes to a shared
	 * constant may segfault
	 */
	bool isSharedConstant() const
	{
		// Shared constants can never have their refcounts changed
		// There's no need for memory ordering here
		return m_refCount.load(std::memory_order_relaxed) == SharedConstantRefCount;
	}

	/**
	 * Returns a writable instance of the byte array
	 *
	 * If the current instance is exclusive it will be returned directly. Otherwise, a new copy will be returned and
	 * the reference count of the existing instance will be decremented.
	 *
	 * In a threaded environment it is unsafe to access the original instance after calling asWritable()
	 *
	 * @param  bytes  Size in bytes of the byte array
	 */
	SharedByteArray *asWritable(size_t bytes)
	{
		if (isExclusive())
		{
			// We have an exclusive copy
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

	/**
	 * Increases the refefence count of the instance
	 *
	 * This should be called before passing the byte array to another user or thread
	 *
	 * @return  Pointer to the current instance
	 */
	SharedByteArray* ref()
	{
		if (isSharedConstant())
		{
			// Don't increment; we're readonly
			return this;
		}

		// We don't need any memory ordering here

		// In the case of refing to pass to another thread is sufficient to make the refcount increment itself visible.
		// In the case of one thread incrementing and then decrementing later the decrement itself will enforce memory
		// ordering. This ensures other threads won't falsely delete the byte array.
		m_refCount.fetch_add(1u, std::memory_order_relaxed);

		return this;
	}

	/**
	 * Decreases the reference count of the instance and deletes it if the caller was holding the last reference
	 *
	 * @return  True if the instance was deleted, false otherwise. In either case it is unsafe to access the instance
	 *          after calling unref()
	 */
	bool unref()
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

	/**
	 * Returns a pointer to the byte array's data
	 *
	 * It is only safe to write to this data after getting a pointer from asWritable(). Once a writable instance has
	 * been passed to another user it's no longer writable
	 */
	const std::uint8_t* data() const
	{
		return &m_data[0];
	}
	
	std::uint8_t* data()
	{
		return &m_data[0];
	}

#ifdef _LLIBY_CHECK_LEAKS
	/**
	 * Returns the number of active SharedByteArray instances
	 *
	 * If leak checking is disabled this always returns 0
	 *
	 * This is not synchronized with other threads. For that reason this value is only accurate when there is no
	 * concurrent instance creation or destruction and any other previously modifying threads have been synchronized with
	 * through another mechanism.
	 */
	static size_t instanceCount();
#else
	static size_t instanceCount()
	{
		return 0;
	}
#endif

private:
	typedef std::uint32_t refcount_t;
	static const std::uint32_t SharedConstantRefCount = std::numeric_limits<refcount_t>::max();

	SharedByteArray(refcount_t initialRefCount) : 
		m_refCount(initialRefCount)
	{
		incrementInstanceCount();
	}

	void operator delete(void *p)
	{
		free(p);
	}

#ifdef _LLIBY_CHECK_LEAKS
	~SharedByteArray();

	static void incrementInstanceCount();
#else
	static void incrementInstanceCount()
	{
	}
#endif

	std::atomic<refcount_t> m_refCount;
	std::uint8_t m_data[];
};

}

#endif
