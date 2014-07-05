#include "Finalizer.h"

#include <cassert>

#include "binding/DatumCell.h"

#include "alloc/AllocCell.h"
#include "alloc/MemoryBlock.h"

namespace lliby
{
namespace alloc
{

void Finalizer::finalizeHeapAsync(MemoryBlock *rootSegment)
{
	std::call_once(mWorkerStartFlag, [=]() {
		// Start the worker thread
		mWorkerThread = std::thread(&Finalizer::workerThread, this);
	});

	// Add to the work queue
	{
		std::lock_guard<std::mutex> guard(mWorkQueueMutex);
		mWorkQueue.push(rootSegment);
	}

	// Notify
	mWorkQueueCond.notify_one();
}

void Finalizer::finalizeHeapSync(MemoryBlock *rootSegment)
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
		finalizeHeapSync(nextSegment);
	}
}

void Finalizer::workerThread()
{
	while(true)
	{
		std::unique_lock<std::mutex> lock(mWorkQueueMutex);

		// Wait for work to do
		mWorkQueueCond.wait(lock, [=]{return !mWorkQueue.empty();});

		// Get the entry
		MemoryBlock *rootSegment = mWorkQueue.front();
		mWorkQueue.pop();

		// Release the lock
		lock.unlock();
		
		finalizeHeapSync(rootSegment);
	}
}

}
}
