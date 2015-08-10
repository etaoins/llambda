#ifndef _LLIBY_BINDING_SHAREDBYTEARRAY_H
#define _LLIBY_BINDING_SHAREDBYTEARRAY_H

#include <cstdlib>
#include <cstdint>

#include <limits>
#include <atomic>

#include "hash/SharedByteHash.h"

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
	using HashValueType = SharedByteHash::ResultType;

	constexpr static std::size_t maximumCapacity()
	{
		// We're limited by the maximum allocation size
		return std::numeric_limits<size_t>::max() - sizeof(SharedByteArray);
	}

	/**
	 * Creates a new SharedByteArray instance of the exactly the given size
	 */
	static SharedByteArray* createInstance(std::size_t bytes);

	/**
	 * Returns the capacity of the SharedByteArray instance in bytes
	 *
	 * This is implemented in terms of platform::mallocActualSize()
	 *
	 * @param  fallbackCapacity  Capacity to return if the SharedByteArray's allocation size cannot be determined.
	 */
	std::size_t capacity(std::size_t fallbackCapacity);

	/**
	 * Destructively resizes this instance and returns a new SharedByteArray of the passed size
	 *
	 * Any existing data is preserved. This is only allowed for exclusive copies non-shared constants.
	 */
	SharedByteArray *destructivelyResizeTo(std::size_t bytes);

	/**
	 * Returns true if the caller has an exclusive copy of the byte array
	 *
	 * Exclusive byte array instances are by definition not a shared constant
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
	SharedByteArray *asWritable(std::size_t bytes);

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
	bool unref();

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

	/**
	 * Returns the hash value for the SharedByteArray data
	 *
	 * This will lazily calculate the value and cache it on the instance.
	 *
	 * @param  bytes  Size in bytes of the byte array
	 *
	 * @sa cachedHashValue()
	 */
	HashValueType hashValue(std::size_t size) const;

	/**
	 * Compares the contents of this byte array with another for equality
	 *
	 * This will may use the hash value of the byte arrays as an optimisation.
	 */
	bool isEqual(const SharedByteArray *other, std::size_t size) const;

	/**
	 * Returns the number of active SharedByteArray instances
	 *
	 * If leak checking is disabled this always returns 0
	 *
	 * This is not synchronized with other threads. For that reason this value is only accurate when there is no
	 * concurrent instance creation or destruction and any other previously modifying threads have been synchronized with
	 * through another mechanism.
	 */
	static std::size_t instanceCount();

private:
	using RefCountType = std::uint32_t;
	static const RefCountType SharedConstantRefCount = std::numeric_limits<RefCountType>::max();

	SharedByteArray(RefCountType initialRefCount);

	void operator delete(void *p)
	{
		free(p);
	}

	void incrementInstanceCount();

#ifdef _LLIBY_CHECK_LEAKS
	~SharedByteArray();
#endif

	std::atomic<RefCountType> m_refCount;
	mutable HashValueType m_cachedHashValue;
	std::uint8_t m_data[];
};

}

#endif
