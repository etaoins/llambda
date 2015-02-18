#include "Finalizer.h"
#include "Heap.h"

#include <cassert>

#include "binding/AnyCell.h"

#include "alloc/AllocCell.h"
#include "alloc/MemoryBlock.h"
#include "sched/Dispatcher.h"

namespace lliby
{
namespace alloc
{

void Finalizer::finalizeHeapAsync(Heap &heap)
{
	if (heap.isEmpty())
	{
		return;
	}

	terminateHeap(heap);

	MemoryBlock *rootSegment = heap.rootSegment();

	sched::Dispatcher::defaultInstance().dispatch([=]() {
		finalizeSegment(rootSegment);
	});

	// Detach all segments from the heap now that we've queued the finalization. This prevents us finalizing the heap
	// again in its destructor
	heap.detach();
}

void Finalizer::finalizeHeapSync(Heap &heap)
{
	if (heap.isEmpty())
	{
		return;
	}

	terminateHeap(heap);
	finalizeSegment(heap.rootSegment());

	heap.detach();
}

void Finalizer::finalizeSegment(MemoryBlock *rootSegment)
{
	auto nextCell = static_cast<AllocCell*>(rootSegment->startPointer());

	while((nextCell->gcState() != GarbageState::HeapTerminator) &&
			(nextCell->gcState() != GarbageState::SegmentTerminator))
	{
		// Make sure our garbage state is valid
		// If a cell is written past its end it can corrupt the garbage state of the next cell
		assert(nextCell->gcState() <= GarbageState::MaximumGarbageState);

		if (nextCell->gcState() != GarbageState::ForwardingCell)
		{
			// This value is no longer referenced
			nextCell->finalize();
		}

		nextCell++;
	}

	MemoryBlock *nextSegment = nullptr;

	if (nextCell->gcState() == GarbageState::SegmentTerminator)
	{
		nextSegment = reinterpret_cast<SegmentTerminatorCell*>(nextCell)->nextSegment();
	}

	// Actually free the block
	delete rootSegment;

	if (nextSegment != nullptr)
	{
		finalizeSegment(nextSegment);
	}
}

void Finalizer::terminateHeap(Heap &heap)
{
	if (heap.m_allocNext != nullptr)
	{
		new (heap.m_allocNext) HeapTerminatorCell();
	}
}

}
}
