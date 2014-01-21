#ifndef _LLIBY_ALLOC_FINALIZER_H
#define _LLIBY_ALLOC_FINALIZER_H

#include <thread>
#include <mutex>
#include <queue>
#include <condition_variable>

namespace lliby
{
namespace alloc
{

class MemoryBlock;

class Finalizer
{
public:
	void finalizeBlockAsync(MemoryBlock *block, void *endPointer);
	static void finalizeBlockSync(MemoryBlock *block, void *endPointer);

private:
	void workerThread();

	struct WorkEntry
	{
		MemoryBlock *block;
		void* endPointer;
	};

	std::thread mWorkerThread;
	std::once_flag mWorkerStartFlag;

	std::mutex mWorkQueueMutex;
	std::condition_variable mWorkQueueCond;
	std::queue<WorkEntry> mWorkQueue;
};

}
}

#endif
