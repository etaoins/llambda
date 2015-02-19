#ifndef _LLIBY_SCHED_DISPATCHER_H
#define _LLIBY_SCHED_DISPATCHER_H

#include <functional>
#include <cstdint>
#include <mutex>
#include <queue>
#include <condition_variable>

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
	~Dispatcher();

	/**
	 * Returns a shared instance of the dispatcher
	 */
	static Dispatcher &defaultInstance();

	/**
	 * Dispatches work on the next available thread
	 */
	void dispatch(const WorkFunction &work);

	/**
	 * Waits for the scheduler to drain all queued work and all worker threads to exit
	 *
	 * Note that this does not prevent new work from being dispatched. This means this function may not make progress
	 * when work is being concurrently dipsatched.
	 *
	 * This is implicitly called by the destructor but it can also be used to checkpoint a running Dispatcher. This is
	 * fairly heavyweight so it should only be used for debugging purposes.
	 */
	void waitForDrain();

private:
	void workerThread(WorkFunction initialWork);

	std::mutex m_mutex;

	std::int32_t m_idleThreads;
	std::int32_t m_runningThreads;

	std::condition_variable m_workQueueCond;
	std::queue<WorkFunction> m_workQueue;

	bool m_draining = false;
	std::condition_variable m_drainCond;
};

}
}

#endif
