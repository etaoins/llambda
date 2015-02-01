#include "Finalizer.h"

#include <cassert>

#include "binding/AnyCell.h"

#include "alloc/AllocCell.h"
#include "alloc/MemoryBlock.h"

namespace lliby
{
namespace alloc
{

void Finalizer::finalizeHeapAsync(MemoryBlock *rootSegment)
{
	if (rootSegment == nullptr)
	{
		return;
	}

	std::call_once(m_workerStartFlag, [=]() {
		// Start the worker thread
		m_workerThread = std::thread(&Finalizer::workerThread, this);
	});

	// Add to the work queue
	{
		std::lock_guard<std::mutex> guard(m_workQueueMutex);
		m_workQueue.push(rootSegment);
	}

	// Notify
	m_workQueueCond.notify_one();
}

void Finalizer::finalizeHeapSync(MemoryBlock *rootSegment)
{
	if (rootSegment == nullptr)
	{
		return;
	}

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
		std::unique_lock<std::mutex> lock(m_workQueueMutex);

		// Wait for work to do
		m_workQueueCond.wait(lock, [=]{return !m_workQueue.empty();});

		// Get the entry
		MemoryBlock *rootSegment = m_workQueue.front();
		m_workQueue.pop();

		// Release the lock
		lock.unlock();

		finalizeHeapSync(rootSegment);
	}
}

}
}
