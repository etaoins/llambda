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
	Heap(std::size_t initialSegementSize);

	/**
	 * Destroys the Heap by synchronously finalizing all cells on the heap
	 *
	 * If asynchronous finalization is preferred then Finalizer::finalizeHeapAsync can be called on the heap before
	 * destructing it.
	 */
	~Heap();

	// These don't make sense
	Heap& operator=(const Heap &) = delete;
	Heap(const Heap &) = delete;

	/**
	 * Allocates the required number of cells as a contiguous memory region
	 *
	 * This cannot fail. The program will be aborted if more memory cannot be allocated
	 */
	void *allocate(std::size_t count = 1)
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

		return allocation;
	}

	/**
	 * Returns the number of allocated cells in the heap since the last call to resetAllocationCounter()
	 */
	std::size_t allocationCounter() const
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
	 * finalization. Any allocations from the passed heap will not be counted towards this heap's allocations counters. 
	 */
	void splice(Heap &other);

	/**
	 * Detaches all memory segments from this heap
	 *
	 * Note this will leak memory unless the segments are otherwise being freed
	 */
	void detach();

private:
	ptrdiff_t currentSegmentAllocations() const
	{
		return m_allocNext - m_currentSegmentStart;
	}

	AllocCell* addNewSegment(std::size_t reserveCount);

	alloc::AllocCell *m_allocNext;
	alloc::AllocCell *m_allocEnd;

	// Size of the next segment to allocate
	// Note that if an oversized segment has been allocated this might not be the actual size of the current segment
	std::size_t m_initialSegmentSize;
	std::size_t m_nextSegmentSize;
	MemoryBlock *m_rootSegment;

	alloc::AllocCell *m_currentSegmentStart;
	std::size_t m_allocationCounterBase;
};

}
}

#endif
