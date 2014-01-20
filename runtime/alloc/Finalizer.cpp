#include "Finalizer.h"

#include "binding/DatumCell.h"

#include "alloc/AllocCell.h"
#include "alloc/MemoryBlock.h"

namespace lliby
{
namespace alloc
{

Finalizer::Finalizer() :
	mWorkerThread(&Finalizer::workerThread, this)
{
}

void Finalizer::finalizeBlockAsync(MemoryBlock *block, void *endPointer)
{
	// Add to the work queue
	{
		std::lock_guard<std::mutex> guard(mWorkQueueMutex);
		mWorkQueue.push(WorkEntry { block, endPointer});
	}

	// Notify
	mWorkQueueCond.notify_one();
}

void Finalizer::finalizeBlockSync(MemoryBlock *block, void *endPointer)
{
	for(auto nextCell = static_cast<AllocCell*>(block->startPointer());
		 nextCell < endPointer;
		 nextCell++)
	{
		if (nextCell->gcState() != GarbageState::ForwardingCell)
		{
			// This value is no longer referenced
			nextCell->finalize();
		}
	}

	// Actually free the block
	delete block;
}

void Finalizer::workerThread()
{
	while(true)
	{
		std::unique_lock<std::mutex> lock(mWorkQueueMutex);

		// Wait for work to do
		mWorkQueueCond.wait(lock, [=]{return !mWorkQueue.empty();});

		// Get the entry
		WorkEntry work = mWorkQueue.front();
		mWorkQueue.pop();

		// Release the lock
		lock.unlock();
		
		finalizeBlockSync(work.block, work.endPointer);
	}
}

}
}
