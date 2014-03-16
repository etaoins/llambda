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
/*
#ifdef _LLIBY_ALWAYS_GC
	// Do one last collection at shutdown
	forceCollection(world);

	if (static_cast<void*>(world.allocNext) != world.activeAllocBlock->startPointer())
	{
		std::cerr << "Cells leaked on exit!" << std::endl;
		exit(-1);
	}
#endif
*/
}
    
void *allocateCells(World &world, size_t count)
{
	return world.cellHeap.allocateCells(count);
}

RangeAlloc allocateRange(World &world, size_t count)
{
	auto start = static_cast<AllocCell*>(allocateCells(world, count));
	auto end = start + count;

	return RangeAlloc(start, end);
}

void forceCollection(World &world)
{
	// Terminate the old cell heap
	world.cellHeap.terminate();

	// Make a new cell heap
	Heap nextCellHeap;

	// Collect in to the new world
	collect(world, nextCellHeap);

	// Finalize the heap asynchronously
	MemoryBlock *rootSegment = world.cellHeap.rootSegment();

	if (rootSegment != nullptr)
	{
		finalizer->finalizeHeapSync(rootSegment);
	}

	// Make this the new heap
	world.cellHeap = nextCellHeap;
}

}
}
