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
	Finalizer *finalizer = nullptr;

#ifndef _LLIBY_ALWAYS_GC
	const size_t MaxAllocBeforeForceGc = 1024 * 1024;
#endif
}

void initGlobal()
{
	finalizer = new Finalizer();
}

void initWorld(World &world)
{
	// Nothing to do
}

void shutdownWorld(World &world)
{
#ifdef _LLIBY_ALWAYS_GC
	// Do one last collection at shutdown
	if (forceCollection(world) > 0)
	{
		std::cerr << "Cells leaked on exit!" << std::endl;
		exit(-1);
	}
#endif
}
    
void *allocateCells(World &world, size_t count)
{
#ifndef _LLIBY_ALWAYS_GC
	if (world.cellHeap.allocationCounter() > MaxAllocBeforeForceGc)
	{
#endif
		forceCollection(world);
#ifndef _LLIBY_ALWAYS_GC
	}
#endif

	return world.cellHeap.allocate(count);
}

RangeAlloc allocateRange(World &world, size_t count)
{
	auto start = static_cast<AllocCell*>(allocateCells(world, count));
	auto end = start + count;

	return RangeAlloc(start, end);
}

size_t forceCollection(World &world)
{
	// Terminate the old cell heap
	world.cellHeap.terminate();

	// Make a new cell heap
	Heap nextCellHeap;

	// Collect in to the new world
	const size_t reachableCells = collect(world, nextCellHeap);

	// Finalize the heap asynchronously
	MemoryBlock *rootSegment = world.cellHeap.rootSegment();

	if (rootSegment != nullptr)
	{
		finalizer->finalizeHeapSync(rootSegment);
	}

	// Don't count the cells we just moved towards the next GC
	nextCellHeap.resetAllocationCounter();

	// Make this the new heap
	world.cellHeap = nextCellHeap;

	return reachableCells;
}

}
}
