#include "CellRefRangeList.h"

namespace lliby
{
namespace alloc
{

void CellRefRangeList::addRange(CellRefRange &range)
{
	// Add to the active list
	range.prev = nullptr;
	range.next = mHead;

	if (mHead)
	{
		mHead->prev = &range;
	}

	mHead = &range;
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
		mHead = range.next;
	}

	if (range.next != nullptr)
	{
		range.next->prev = range.prev;
	}
}

}
}
