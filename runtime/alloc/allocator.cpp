#include "alloc/allocator.h"

#include <stdlib.h>
#include <iostream>

#include "core/World.h"

#include "alloc/AllocCell.h"
#include "alloc/RangeAlloc.h"
#include "alloc/Finalizer.h"
#include "alloc/DynamicMemoryBlock.h"
#include "alloc/collector.h"

// Statically check that everything can fit in to a cell
#include "generated/sizecheck.h"

extern "C"
{

using lliby::alloc::AllocCell;

// These are used directly by generated code to avoid function call overhead

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

	if (static_cast<void*>(world.allocNext) != world.activeAllocBlock->startPointer())
	{
		std::cerr << "Cells leaked on exit!" << std::endl;
		exit(-1);
	}
#endif
}
    
void *allocateCells(World &world, size_t count)
{
	AllocCell *allocation = world.allocNext;
	AllocCell *newAllocNext = allocation + count;

	if (newAllocNext > world.allocEnd)
	{
		// Our allocation goes past the end - collect garbage
		if (!forceCollection(world, count))
		{
			std::cerr << "GC space exhausted" << std::endl;
			exit(-2);
		}
	
		// If forceCollection() returned true this must succeed
		allocation = world.allocNext;
		newAllocNext = allocation + count;
	}

	// Mark the cells as allocated
	for(size_t i = 0; i < count; i++)
	{
		allocation[i].setGcState(GarbageState::AllocatedCell);
	}

	// Start allocating past the end of this allocation
	world.allocNext = newAllocNext;

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
	auto newBlock = new DynamicMemoryBlock(SemiSpaceSize);

	if (!newBlock->isValid())
	{
		std::cerr << "Unable to allocate " << SemiSpaceSize << " bytes" << std::endl;
		exit(-2);
	}

	if (oldBlock != nullptr)
	{
		// This indicates the end of the allocated cell range
		auto oldAllocNext = world.allocNext;

		// Garbage collect if this isn't our first allocation
		world.allocNext = static_cast<AllocCell*>(alloc::collect(world, oldBlock->startPointer(), oldAllocNext, newBlock->startPointer()));

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
		world.allocNext = static_cast<AllocCell*>(newBlock->startPointer());
	}

	// Set up the new pointers
	auto semiSpaceEnd = reinterpret_cast<AllocCell*>(newBlock->endPointer());
	world.activeAllocBlock = newBlock;

#ifndef _LLIBY_ALWAYS_GC
	world.allocEnd = semiSpaceEnd;
	
	// Make sure the reserved space will fit 
	return (world.allocNext + reserveCount) <= world.allocEnd;
#else	
	// This will trigger the GC again on the next allocation. This will break GC unsafe code at every allocation point
	// which is useful for shaking out bugs.
	world.allocEnd = world.allocNext + reserveCount;

	return world.allocEnd <= semiSpaceEnd;
#endif
}

}
}
