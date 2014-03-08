#include "CellRefRangeList.h"

#include <iostream>

#include "DynamicMemoryBlock.h"

namespace lliby
{
namespace alloc
{

namespace
{
	const size_t MemoryBlockSize = 4096;

	void removeRangeNode(CellRefRange *range, CellRefRange *&listHead)
	{
		if (range->prev != nullptr)
		{
			range->prev->next = range->next;
		}
		else
		{
			// We have no previous element; we must be the head
			listHead = range->next;
		}

		if (range->next != nullptr)
		{
			range->next->prev = range->prev;
		}
	}

	void addRangeNode(CellRefRange *range, CellRefRange *&listHead)
	{
		range->prev = nullptr;
		range->next = listHead;

		if (listHead)
		{
			listHead->prev = range;
		}

		listHead = range;
	}
}

CellRefRangeList::CellRefRangeList() : mActiveHead(nullptr)
{
	mBackingBlock = new DynamicMemoryBlock(MemoryBlockSize);

	const size_t rangeCount = MemoryBlockSize / sizeof(CellRefRange); 
	auto backingRanges = reinterpret_cast<CellRefRange*>(mBackingBlock->startPointer());

	CellRefRange *prevRange = nullptr;
	for(size_t i = 0; i < rangeCount; i++)
	{
		CellRefRange *range = &backingRanges[i];

		range->prev = prevRange;
		range->next = &backingRanges[i + 1]; 

		prevRange = range;
	}

	prevRange->next = nullptr;

	mFreeHead = backingRanges;
}

CellRefRangeList::~CellRefRangeList()
{
	delete mBackingBlock;
}

CellRefRange* CellRefRangeList::addRange(AllocCell **basePointer, size_t cellCount)
{
	if (mFreeHead == nullptr)
	{
		std::cerr << "Ran out of space for GC roots" << std::endl;
		abort();
	}

	// Take this from the head of the free list
	CellRefRange *newRange = mFreeHead;
	mFreeHead = newRange->next;

	// Update the inner values
	newRange->basePointer = basePointer;
	newRange->cellCount = cellCount;

	// Add to the active list
	addRangeNode(newRange, mActiveHead);

	return newRange;
}

void CellRefRangeList::removeRange(CellRefRange *range)
{
	removeRangeNode(range, mActiveHead);
	addRangeNode(range, mFreeHead);
}

}
}
