#ifndef _LLIBY_SCHED_DISPATCHER_H
#define _LLIBY_SCHED_DISPATCHER_H

#include <functional>
#include <cstdint>
#include <mutex>
#include <queue>
#include <condition_variable>
#include <atomic>

namespace lliby
{
namespace sched
{

class Dispatcher
{
public:
	using WorkFunction = std::function<void()>;

	/**
	 * Creates a new standlone dispatcher
	 */
	Dispatcher();

	/**
	 * Returns a shared instance of the dispatcher
	 */
	static Dispatcher &defaultInstance();

	/**
	 * Dispatches work on the next available thread
	 */
	void dispatch(const WorkFunction &work);

private:
	void workerThread(WorkFunction initialWork);

	std::mutex m_mutex;

	std::int32_t m_idleThreads;
	std::condition_variable m_workQueueCond;
	std::queue<WorkFunction> m_workQueue;

#ifdef _LLIBY_CHECK_LEAKS
public:
	/**
	 * Waits for the scheduler to drain all queued work
	 */
	void waitForDrain();

private:
	std::atomic<int32_t> m_runningThreads;
	std::condition_variable m_drainCond;
#endif
};

}
}

#endif
