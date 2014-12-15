#include "alloc/allocator.h"

#include <stdlib.h>
#include <iostream>

#include "core/World.h"

#include "binding/RecordLikeCell.h"
#include "binding/SharedByteArray.h"

#include "alloc/AllocCell.h"
#include "alloc/RangeAlloc.h"
#include "alloc/Finalizer.h"
#include "alloc/DynamicMemoryBlock.h"
#include "alloc/collector.h"

// Statically check that everything can fit in to a cell
#include "generated/sizecheck.h"

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
#ifdef _LLIBY_CHECK_LEAKS
	// Do one last collection at shutdown
	if (forceCollection(world) > 0)
	{
		std::cerr << "Cells leaked on exit!" << std::endl;
		exit(-1);
	}

	if (SharedByteArray::instanceCount() != 0)
	{
		std::cerr << "SharedByteArray instances leaked on exit!" << std::endl;
		exit(-1);
	}
	
	if (RecordLikeCell::recordDataInstanceCount() != 0)
	{
		std::cerr << "Record data instances leaked on exit!" << std::endl;
		exit(-1);
	}
#endif
}
    
AllocCell *allocateCells(World &world, size_t count)
{
#ifndef _LLIBY_ALWAYS_GC
	if (world.cellHeap.allocationCounter() > MaxAllocBeforeForceGc)
	{
#endif
		forceCollection(world);
#ifndef _LLIBY_ALWAYS_GC
	}
#endif

	return static_cast<AllocCell*>(world.cellHeap.allocate(count));
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
		/* We can normally finalize memory in a background thread for better concurrency. There are two debug flags
		 * which require synchronous finalization:
		 *
		 * - NO_ADDR_REUSE immediately marks the memory as inaccessible which means the finalizer must be done with it
		 *   before we return from collection.
		 *
		 * - CHECK_LEAKS needs all finalization to be complete at exit. Currently there's no way to wait for the
		 *   finalizer queue to drain so it makes all finalization synchronous.
		 */
#if !defined(_LLIBY_NO_ADDR_REUSE) && !defined(_LLIBY_CHECK_LEAKS)
		finalizer->finalizeHeapAsync(rootSegment);
#else
		finalizer->finalizeHeapSync(rootSegment);
#endif
	}

	// Don't count the cells we just moved towards the next GC
	nextCellHeap.resetAllocationCounter();

	// Make this the new heap
	world.cellHeap = nextCellHeap;

	return reachableCells;
}

}
}
