#include "alloc/Heap.h"

#include <iostream>
#include <stdlib.h>

#include "alloc/MemoryBlock.h"
#include "alloc/Finalizer.h"

namespace lliby
{
namespace alloc
{

namespace
{
	const std::size_t SegmentGrowthFactor = 4;
	const std::size_t SegmentMaximumSize = 1 * 1024 * 1024;
}

Heap::Heap(std::size_t initialSegmentSize)
	: m_initialSegmentSize(initialSegmentSize)
{
	detach();
}

void Heap::detach()
{
	m_allocNext = nullptr;
	m_allocEnd = nullptr;

	m_nextSegmentSize = m_initialSegmentSize;
	m_rootSegment = nullptr;

	m_currentSegmentStart = nullptr;
	m_allocationCounterBase = 0;
}

Heap::~Heap()
{
	Finalizer::finalizeHeapSync(*this);
}

AllocCell* Heap::addNewSegment(std::size_t reserveCount)
{
	const std::size_t minimumBytes = (sizeof(AllocCell) * reserveCount) + sizeof(SegmentTerminatorCell);
	std::size_t newSegmentSize;

	if (minimumBytes > m_nextSegmentSize)
	{
		// This is a huge allocation
		// Allocate it all at once
		newSegmentSize = minimumBytes;
	}
	else
	{
		// This will fit in a normal segment
		newSegmentSize = m_nextSegmentSize;

		// Update our next segment size
		m_nextSegmentSize = std::min(m_nextSegmentSize * SegmentGrowthFactor, SegmentMaximumSize);
	}

	// This will update m_allocNext/m_allocEnd
	auto newSegment = MemoryBlock::create(newSegmentSize);

	if (m_rootSegment == nullptr)
	{
		m_rootSegment = newSegment;
	}
	else
	{
		// Add a pointer to this new segment at the end of the old segment
		new (m_allocNext) SegmentTerminatorCell(newSegment);

		// Track the number of allocations made in the previous segment
		m_allocationCounterBase += currentSegmentAllocations();
	}

	m_currentSegmentStart = static_cast<AllocCell*>(newSegment->startPointer());

	m_allocNext = m_currentSegmentStart + reserveCount;

	// Find the number of cells we can fit in the segment with room for a segment terminator
	const size_t usableCellCount = (newSegment->size(newSegmentSize) - sizeof(SegmentTerminatorCell)) / sizeof(AllocCell);

	m_allocEnd = reinterpret_cast<AllocCell*>(newSegment->startPointer()) + usableCellCount;

	return m_currentSegmentStart;
}

void Heap::splice(Heap &other)
{
	if (other.m_rootSegment == nullptr)
	{
		// Empty heap; nothing to splice
		return;
	}

	// Remember our old root segment
	MemoryBlock *oldRoot = m_rootSegment;

	// Steal the other heap's root
	m_rootSegment = other.m_rootSegment;

	if (oldRoot != nullptr)
	{
		// Point the last segment of the passed heap to the beginning of our heap
		new (other.m_allocNext) SegmentTerminatorCell(oldRoot);
	}
	else
	{
		// Take over the heap's allocation state
		// Intentionally don't copy m_initialSegmentSize - this is a per-Heap tuning value
		m_allocNext = other.m_allocNext;
		m_allocEnd = other.m_allocEnd;
		m_nextSegmentSize = other.m_nextSegmentSize;
		m_currentSegmentStart = other.m_currentSegmentStart;
		m_allocationCounterBase = -currentSegmentAllocations();
	}

	// Destroy the other heap for safety
	other.detach();
}

}
}
