#ifndef _LLIBY_UTIL_SHAREDBYTEARRAY_H
#define _LLIBY_UTIL_SHAREDBYTEARRAY_H

#include <cstdlib>
#include <cstdint>

#include <limits>
#include <atomic>

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
	static SharedByteArray* createInstance(std::size_t bytes);

	/**
	 * Creates a new SharedByteArray instance of at least the given size
	 *
	 * This uses lliby::sizedMalloc() internally to determine how much slack allocation was provided by the platform's
	 * malloc() implementation
	 *
	 * @param  bytes  Minimum size in types. On a successful return this will be the usable size in bytes of the byte
	 *                array.
	 */
	static SharedByteArray *createMinimumSizedInstance(std::size_t &bytes);

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
	SharedByteArray* ref();

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
	typedef std::uint32_t refcount_t;
	static const std::uint32_t SharedConstantRefCount = std::numeric_limits<refcount_t>::max();

	SharedByteArray(refcount_t initialRefCount);

	void operator delete(void *p)
	{
		free(p);
	}

	void incrementInstanceCount();

#ifdef _LLIBY_CHECK_LEAKS
	~SharedByteArray();
#endif

	std::atomic<refcount_t> m_refCount;
	std::uint8_t m_data[];
};

}

#endif
