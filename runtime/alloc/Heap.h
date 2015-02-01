#ifndef _LLIBY_ALLOC_HEAP_H
#define _LLIBY_ALLOC_HEAP_H

#include <cstdint>
#include <cstddef>

#include "alloc/AllocCell.h"

namespace lliby
{
namespace alloc
{

class MemoryBlock;
class RangeAlloc;
class AllocCell;
class Finalizer;

/**
 * Represents a heap that cells can be allocated from
 *
 * This abstracts a segmented stack-like heap where memory blocks are allocated on demand to meet new allocations.
 */
class Heap
{
	friend class Finalizer;
public:
	/**
	 * Creates a new heap
	 *
	 * This does not allocate any memory; it initializes a completely empty heap
	 */
	Heap();

	/**
	 * Allocates the required number of cells as a contiguous memory region
	 *
	 * This cannot fail. The program will be aborted if more memory cannot be allocated
	 */
	void *allocate(size_t count = 1)
	{
		AllocCell *allocation = m_allocNext;
		AllocCell *newAllocNext = allocation + count;

		if (newAllocNext > m_allocEnd)
		{
			// This adds a new segment and updates m_allocNext
			allocation = addNewSegment(count);
		}
		else
		{
			m_allocNext = newAllocNext;
		}

		// GarbageState::AllocatedCell is 0 so we don't need to set it explicitly

		return allocation;
	}

	/**
	 * Returns the number of allocated cells in the heap since the last call to resetAllocationCounter()
	 */
	size_t allocationCounter() const
	{
		return currentSegmentAllocations() + m_allocationCounterBase;
	}

	/**
	 * Resets the allocation counter
	 *
	 * \sa allocationCounter
	 */
	void resetAllocationCounter()
	{
		m_allocationCounterBase = -currentSegmentAllocations();
	}

	/**
	 * Returns true if the heap is empty
	 */
	bool isEmpty() const
	{
		return m_rootSegment == nullptr;
	}

	/**
	 * Returns the root segment of the heap
	 *
	 * This is used by the finalizer to walk over all of the heap segments
	 */
	MemoryBlock* rootSegment() const
	{
		return m_rootSegment;
	}

	/**
	 * Destructively splices the contents of the passed heap in to this heap
	 *
	 * This heap will own all of the memory segments of the passed heap and they will be iterated over during
	 * finalisation. The passed heap must not be terminated
	 */
	void splice(Heap &other);

#ifdef _LLIBY_ALWAYS_GC
	/**
	 * Forces a new segment to be allocated on the next allocation
	 */
	void sealCurrentSegment()
	{
		m_allocNext = m_allocEnd;
	}
#endif

private:
	void terminate();

	ptrdiff_t currentSegmentAllocations() const
	{
		return m_allocNext - m_currentSegmentStart;
	}

	AllocCell* addNewSegment(size_t reserveCount);

	alloc::AllocCell *m_allocNext = nullptr;
	alloc::AllocCell *m_allocEnd = nullptr;

	// Size of the next segment to allocate
	// Note that if an oversized segment has been allocated this might not be the actual size of the current segment
	uint64_t m_nextSegmentSize;
	MemoryBlock *m_rootSegment = nullptr;

	alloc::AllocCell *m_currentSegmentStart = nullptr;
	std::int64_t m_allocationCounterBase = 0;
};

}
}

#endif
