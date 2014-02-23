#include "alloc/allocator.h"

#include <stdlib.h>
#include <iostream>

#include "core/World.h"

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

void *_lliby_alloc_cells(lliby::World &world, std::uint64_t count)
{
	return lliby::alloc::allocateCells(world, count);
}

}

namespace lliby
{
namespace alloc
{

namespace
{
	const size_t SemiSpaceSize = 4 * 1024 * 1024; 
	Finalizer *finalizer = nullptr;
}

void initGlobal()
{
	finalizer = new Finalizer();
}

void initWorld(World &world)
{
	// This will create our initial semispace
	forceCollection(world);
}

void shutdownWorld(World &world)
{
#ifdef _LLIBY_ALWAYS_GC
	// Do one last collection at shutdown
	forceCollection(world);

	if (static_cast<void*>(_lliby_alloc_next) != world.activeAllocBlock->startPointer())
	{
		std::cerr << "Cells leaked on exit!" << std::endl;
		exit(-1);
	}
#endif
}
    
void *allocateCells(World &world, size_t count)
{
	AllocCell *allocation = _lliby_alloc_next;
	AllocCell *newAllocNext = allocation + count;

	if (newAllocNext > _lliby_alloc_end)
	{
		// Our allocation goes past the end - collect garbage
		if (!forceCollection(world, count))
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

RangeAlloc allocateRange(World &world, size_t count)
{
	auto start = static_cast<AllocCell*>(allocateCells(world, count));
	auto end = start + count;

	return RangeAlloc(start, end);
}

bool forceCollection(World &world, size_t reserveCount)
{
	// Directly allocate memory from the kernel
	MemoryBlock *oldBlock = world.activeAllocBlock;
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
		_lliby_alloc_next = static_cast<AllocCell*>(alloc::collect(world, oldBlock->startPointer(), oldAllocNext, newBlock->startPointer()));

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
	world.activeAllocBlock = newBlock;

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
