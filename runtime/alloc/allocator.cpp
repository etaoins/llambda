#include "alloc/allocator.h"

#include <stdlib.h>
#include <iostream>

#include "alloc/AllocCell.h"
#include "alloc/RangeAlloc.h"
#include "alloc/Finalizer.h"
#include "alloc/MemoryBlock.h"
#include "alloc/collector.h"

// Statically check that everything can fit in to a cell
#include "generated/sizecheck.h"

extern "C"
{

using lliby::alloc::AllocCell;

// These are used directly by generated code to avoid function call overhead

// Pointer to the next cell to be allocated
AllocCell *_lliby_alloc_next = nullptr;
// Pointer to the end of the last available allocation cell
AllocCell *_lliby_alloc_end = nullptr;

void *_lliby_alloc_cells(std::uint64_t count)
{
	return lliby::alloc::allocateCells(count);
}

}

namespace lliby
{
namespace alloc
{

namespace
{
	const size_t SemiSpaceSize = 4 * 1024 * 1024; 

	// Pointer to the start of the semi-space
	MemoryBlock *activeBlock = nullptr;
	Finalizer *finalizer = nullptr;
}

void init()
{
	// This will create our initial semispace
	forceCollection();

	finalizer = new Finalizer();
}
    
void *allocateCells(size_t count)
{
	AllocCell *allocation = _lliby_alloc_next;
	AllocCell *newAllocNext = allocation + count;

	if (newAllocNext > _lliby_alloc_end)
	{
		// Our allocation goes past the end - collect garbage
		if (!forceCollection(count))
		{
			std::cerr << "GC space exhausted" << std::endl;
			exit(-2);
		}
	
		// If forceCollection() returned true this must succeed
		allocation = _lliby_alloc_next;
		newAllocNext = allocation + count;
	}

	// Mark the cells as allocated
	for(size_t i = 0; i < count; i++)
	{
		allocation[i].setGcState(GarbageState::AllocatedCell);
	}

	// Start allocating past the end of this allocation
	_lliby_alloc_next = newAllocNext;

	return allocation;
}

RangeAlloc allocateRange(size_t count)
{
	auto start = static_cast<AllocCell*>(allocateCells(count));
	auto end = start + count;

	return RangeAlloc(start, end);
}

bool forceCollection(size_t reserveCount)
{
	// Directly allocate memory from the kernel
	MemoryBlock *oldBlock = activeBlock;
	MemoryBlock *newBlock = new MemoryBlock(SemiSpaceSize);

	if (!newBlock->isValid())
	{
		std::cerr << "Unable to allocate " << SemiSpaceSize << " bytes" << std::endl;
		exit(-2);
	}

	if (oldBlock != nullptr)
	{
		// This indicates the end of the allocated cell range
		auto oldAllocNext = _lliby_alloc_next;

		// Garbage collect if this isn't our first allocation
		_lliby_alloc_next = static_cast<AllocCell*>(alloc::collect(oldBlock->startPointer(), oldAllocNext, newBlock->startPointer()));

#ifndef _LLIBY_NO_ADDR_REUSE
		// Finalize the block asynchronously
		finalizer->finalizeBlockAsync(oldBlock, oldAllocNext);
#else
		// Finalize the block immediately to invalidate the old addresses
		finalizer->finalizeBlockSync(oldBlock, oldAllocNext);
#endif
	}
	else 
	{
		// No previous allocation; we can start at the beginning of the block
		_lliby_alloc_next = static_cast<AllocCell*>(newBlock->startPointer());
	}

	// Set up the new pointers
	auto semiSpaceEnd = reinterpret_cast<AllocCell*>(newBlock->endPointer());
	activeBlock = newBlock;

#ifndef _LLIBY_ALWAYS_GC
	_lliby_alloc_end = semiSpaceEnd;
	
	// Make sure the reserved space will fit 
	return (_lliby_alloc_next + reserveCount) <= _lliby_alloc_end;
#else	
	// This will trigger the GC again on the next allocation. This will break GC unsafe code at every allocation point
	// which is useful for shaking out bugs.
	_lliby_alloc_end = _lliby_alloc_next + reserveCount;

	return _lliby_alloc_end <= semiSpaceEnd;
#endif
}

}
}
