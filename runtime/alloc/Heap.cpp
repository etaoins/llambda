#include "alloc/Heap.h"

#include <iostream>
#include <stdlib.h>

#include "alloc/MemoryBlock.h"
#include "alloc/RangeAlloc.h"
#include "alloc/DynamicMemoryBlock.h"

namespace lliby
{
namespace alloc
{

namespace
{
	const uint64_t SegmentInitialSize = 4 * 1024;
	const uint64_t SegmentGrowthFactor = 4;
	const uint64_t SegmentMaximumSize = 1 * 1024 * 1024;
}

Heap::Heap() :
	m_nextSegmentSize(SegmentInitialSize)
{
}
	
void Heap::terminate()
{
	new (m_allocNext) HeapTerminatorCell();
}

AllocCell* Heap::addNewSegment(size_t reserveCount)
{
	// We ran out of space
	const size_t minimumBytes = (sizeof(AllocCell) * reserveCount) + sizeof(SegmentTerminatorCell);
	size_t newSegmentSize;

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
	auto newSegment = new DynamicMemoryBlock(newSegmentSize);

	if (!newSegment->isValid())
	{
		std::cerr << "Unable to allocate " << newSegmentSize << " bytes" << std::endl;
		exit(-2);
	}

	if (m_rootSegment == nullptr)
	{
		m_rootSegment = newSegment;
	}
	else
	{
		// Add a pointer to this new segment at the end of the old segment
		new (m_allocNext) SegmentTerminatorCell(newSegment);

		// Track the number of allocations made in the previous segment
		m_previousAllocations += m_allocNext - m_currentSegmentStart;
	}

	m_currentSegmentStart = static_cast<AllocCell*>(newSegment->startPointer());

	m_allocNext = m_currentSegmentStart + reserveCount;
	m_allocEnd = reinterpret_cast<AllocCell*>(static_cast<char*>(newSegment->endPointer()) - sizeof(SegmentTerminatorCell)); 

	return m_currentSegmentStart;
}

}
}
