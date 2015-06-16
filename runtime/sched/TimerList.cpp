#include "sched/TimerList.h"
#include "sched/Dispatcher.h"

namespace lliby
{
namespace sched
{

TimerList::TimerList()
{
	// Now that we're initialised start the fire thread
	m_fireThread = std::thread(&TimerList::fireThreadLoop, this);
}

TimerList::~TimerList()
{
	// We need to make sure our fire thread is shut down before we free ourselves
	m_requestShutdown = true;
	m_earlyWakeCond.notify_one();

	m_fireThread.join();
}

TimerList& TimerList::defaultInstance()
{
	static TimerList instance;
	return instance;
}

void TimerList::fireThreadLoop()
{
	std::unique_lock<std::mutex> locker(m_mutex);

	while(true)
	{
		if (m_requestShutdown)
		{
			// No longer need to run
			break;
		}

		auto now = Clock::now();

		// Fire all expired timers
		while(m_timerQueue.size() && (m_timerQueue.top().fireTime <= now))
		{
			auto &toFire = m_timerQueue.top();

			toFire.work();
			m_timerQueue.pop();
		}

		if (!m_timerQueue.size())
		{
			// Nothing to wait on
			m_earlyWakeCond.wait(locker);
		}
		else
		{
			m_earlyWakeCond.wait_until(locker, m_timerQueue.top().fireTime);
		}
	}
}

void TimerList::enqueueDelayedWork(const WorkFunction &work, Clock::duration delay)
{
	bool needsEarlyWake;
	Clock::time_point fireTime = Clock::now() + delay;

	{
		std::unique_lock<std::mutex> locker(m_mutex);

		if (m_timerQueue.size() == 0)
		{
			// There's no enqueued work; perform an early wakeup
			needsEarlyWake = true;
		}
		else
		{
			// The next enqueued fire is before this fire; perform an early wakeup
			needsEarlyWake = m_timerQueue.top().fireTime < fireTime;
		}

		m_timerQueue.emplace(DelayedWork{work, fireTime});
	}

	if (needsEarlyWake)
	{
		m_earlyWakeCond.notify_one();
	}
}

}
}
