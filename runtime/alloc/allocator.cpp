#include "alloc/allocator.h"

#include <cstdlib>

#include "core/World.h"

#include "alloc/AllocCell.h"
#include "alloc/RangeAlloc.h"
#include "alloc/Finalizer.h"
#include "alloc/collector.h"

#ifdef _LLIBY_CHECK_LEAKS
#include <iostream>

#include "sched/Dispatcher.h"
#include "binding/RecordLikeCell.h"
#include "binding/SharedByteArray.h"
#include "actor/Mailbox.h"
#endif

// Statically check that everything can fit in to a cell
#include "generated/sizecheck.h"

namespace lliby
{
namespace alloc
{

namespace
{
#ifndef _LLIBY_ALWAYS_GC
	const std::size_t MaxAllocBeforeForceGc = 1024 * 1024;
#endif
}

void reportGlobalLeaks()
{
#ifdef _LLIBY_CHECK_LEAKS
	sched::Dispatcher::defaultInstance().waitForDrain();

	// Make sure everything is properly freed
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

	if (actor::Mailbox::instanceCount())
	{
		std::cerr << "Actor mailboxes leaked on exit!" << std::endl;
		exit(-1);
	}
#endif
}

AllocCell *allocateCells(World &world, std::size_t count)
{
	return static_cast<AllocCell*>(world.cellHeap.allocate(count));
}

RangeAlloc allocateRange(World &world, std::size_t count)
{
	auto start = static_cast<AllocCell*>(allocateCells(world, count));
	auto end = start + count;

	return RangeAlloc(start, end);
}

void conditionalCollection(World &world)
{
#ifndef _LLIBY_ALWAYS_GC
	if (world.cellHeap.allocationCounter() > MaxAllocBeforeForceGc)
	{
#endif
		forceCollection(world);
#ifndef _LLIBY_ALWAYS_GC
	}
#endif
}

std::size_t forceCollection(World &world)
{
	// Make a new cell heap
	Heap nextCellHeap(World::InitialHeapSegmentSize);

	// Collect in to the new world
	const std::size_t reachableCells = collect(world, nextCellHeap);

	/* We can normally finalize memory in a background thread for better concurrency. However,  ALWAYS_GC
	 * immediately marks the memory as inaccessible which means the finalizer must be done with it before we return
	 * from collection.
	 */
#if !defined(_LLIBY_ALWAYS_GC)
	Finalizer::finalizeHeapAsync(world.cellHeap);
#else
	Finalizer::finalizeHeapSync(world.cellHeap);
#endif

	// The finalizer should've emptied us
	assert(world.cellHeap.isEmpty());

	// Splice in the new cells
	world.cellHeap.splice(nextCellHeap);

	// We should have zero allocation counter now
	assert(world.cellHeap.allocationCounter() == 0);

	return reachableCells;
}

}
}
