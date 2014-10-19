#include "CellRefRangeList.h"

#include <cassert>

namespace lliby
{
namespace alloc
{

namespace
{
	void relocateCellRefRangePtr(CellRefRange *&cellRefRange, std::ptrdiff_t offset)
	{
		if (cellRefRange != nullptr)
		{
			// Cast to char* so we do byte-based pointer arithmetic
			cellRefRange = reinterpret_cast<CellRefRange*>(reinterpret_cast<char*>(cellRefRange) + offset);
		}
	}
}

void CellRefRangeList::addRange(CellRefRange &range)
{
#ifndef NDEBUG
	// Ensure this hasn't already been rooted
	for(auto existingRange = head();
		existingRange != nullptr;
		existingRange = existingRange->next)
	{
		void *newStart = range.basePointer;
		void *newEnd = range.basePointer + range.cellCount;

		void *existingStart = existingRange->basePointer;
		void *existingEnd = existingRange->basePointer + existingRange->cellCount;

		if (((newStart >= existingStart) && (newStart < existingEnd)) ||
			((newEnd > existingStart) && (newEnd <= existingEnd)))
		{
			assert(false);
		}
	}
#endif

	// Add to the active list
	range.prev = nullptr;
	range.next = m_head;

	if (m_head)
	{
		m_head->prev = &range;
	}

	m_head = &range;
}

void CellRefRangeList::removeRange(CellRefRange &range)
{
	if (range.prev != nullptr)
	{
		range.prev->next = range.next;
	}
	else
	{
		// We have no previous element; we must be the head
		m_head = range.next;
	}

	if (range.next != nullptr)
	{
		range.next->prev = range.prev;
	}
}

void CellRefRangeList::relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd)
{
	relocateCellRefRangePtr(m_head, offset);

	for(auto cellRefRange = head();
		cellRefRange != nullptr;
		cellRefRange = cellRefRange->next)
	{
		relocateCellRefRangePtr(cellRefRange->prev, offset);
		relocateCellRefRangePtr(cellRefRange->next, offset);

		if ((cellRefRange->basePointer >= oldStackStart) && (cellRefRange->basePointer < oldStackEnd))
		{
			cellRefRange->basePointer = reinterpret_cast<AllocCell**>(reinterpret_cast<char*>(cellRefRange->basePointer) + offset);
		}
	}
}

}
}
