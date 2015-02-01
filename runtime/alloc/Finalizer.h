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

class Heap;
class MemoryBlock;

class Finalizer
{
public:
	void finalizeHeapAsync(Heap &heap);
	static void finalizeHeapSync(Heap &heap);

private:
	static void finalizeSegment(MemoryBlock *rootSegment);
	static void terminateHeap(Heap &heap);

	void workerThread();

	std::thread m_workerThread;
	std::once_flag m_workerStartFlag;

	std::mutex m_workQueueMutex;
	std::condition_variable m_workQueueCond;
	std::queue<MemoryBlock*> m_workQueue;
};

}
}

#endif
