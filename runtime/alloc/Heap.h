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

/**
 * Represents a heap that cells can be allocated from
 *
 * This abstracts a segmented stack-like heap where memory blocks are allocated on demand to meet new allocations.
 */
class Heap
{
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
	void *allocateCells(size_t count = 1)
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

		// Mark the cells as allocated
		for(size_t i = 0; i < count; i++)
		{
			allocation[i].setGcState(GarbageState::AllocatedCell);
		}

		return allocation;
	}

	/**
	 * Returns the total number of allocated cells in the heap
	 */
	size_t totalAllocations() const
	{
		return (m_allocNext - m_currentSegmentStart) + m_previousAllocations;
	}

	/**
	 * Places a heap terminator at the end of the current segment
	 *
	 * This must be called before finalizing the heap
	 */
	void terminate();

	/**
	 * Returns the root segment of the heap
	 *
	 * This is used by the finalizer to walk over all of the heap segments
	 */
	MemoryBlock* rootSegment() const
	{
		return m_rootSegment;
	}

private:
	AllocCell* addNewSegment(size_t reserveCount);

	alloc::AllocCell *m_allocNext = nullptr;
	alloc::AllocCell *m_allocEnd = nullptr;

	// Size of the next segment to allocate
	// Note that if an oversized segment has been allocated this might not be the actual size of the current segment
	uint64_t m_nextSegmentSize;
	MemoryBlock *m_rootSegment = nullptr;

	alloc::AllocCell *m_currentSegmentStart = nullptr;
	std::uint64_t m_previousAllocations = 0;
};

}
}

#endif
