#ifndef _LLIBY_SCHED_TIMERLIST_H
#define _LLIBY_SCHED_TIMERLIST_H

#include <queue>
#include <functional>
#include <chrono>
#include <mutex>
#include <condition_variable>
#include <vector>
#include <thread>

namespace lliby
{
namespace sched
{

/**
 * Basic timer list implementation
 *
 * This is very simple without support for repeating timers or cancellation.
 */
class TimerList
{
public:
	/**
	 * Standard library clock used to schedule work
	 */
	using Clock = std::chrono::steady_clock;

	using WorkFunction = std::function<void()>;

	/**
	 * Creates a new timer list
	 *
	 * This will create a dedicated timer thread that will live for the duration of the instance.
	 */
	TimerList();

	~TimerList();

	/**
	 * Returns a shared instance of the timer list
	 */
	static TimerList &defaultInstance();

	/**
	 * Enqueues work to be run at a later time
	 *
	 * @param  work   Work function to be called after the specified delay. This is called in shared timer callback
	 *                thread with the timer list lock held. For this reason any work taking more than a trivial amount
	 *                of time should be triggered asynchronously from this function.
	 * @param  delay  Amount of time to delay the call of the work function for.
	 */
	void enqueueDelayedWork(const WorkFunction &work, Clock::duration delay);

private:
	struct DelayedWork
	{
		WorkFunction work;
		Clock::time_point fireTime;
		std::uint64_t timerId;
	};

	class CompareWork
	{
	public:
		bool operator()(const DelayedWork &a, const DelayedWork &b)
		{
			// This orders the work so the next firing time is the head of the queue
			return a.fireTime > b.fireTime;
		}
	};

	void fireThreadLoop();

	std::mutex m_mutex;
	std::priority_queue<DelayedWork, std::vector<DelayedWork>, CompareWork> m_timerQueue;
	std::condition_variable m_earlyWakeCond;

	bool m_requestShutdown = false;
	std::thread m_fireThread;
};

}
}

#endif
