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
	void finalizeHeapAsync(MemoryBlock *rootSegment);
	static void finalizeHeapSync(MemoryBlock *rootSegment);

private:
	void workerThread();

	std::thread mWorkerThread;
	std::once_flag mWorkerStartFlag;

	std::mutex mWorkQueueMutex;
	std::condition_variable mWorkQueueCond;
	std::queue<MemoryBlock*> mWorkQueue;
};

}
}

#endif
