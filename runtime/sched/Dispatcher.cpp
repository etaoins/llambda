#include "sched/Dispatcher.h"

#include <thread>
#include <chrono>

namespace lliby
{
namespace sched
{

Dispatcher::Dispatcher() :
	m_idleThreads(0)
{
}

Dispatcher& Dispatcher::defaultInstance()
{
	// Static initialisation is thread safe in C++11
	static Dispatcher inst;
	return inst;
}

void Dispatcher::dispatch(const WorkFunction &work)
{
	std::unique_lock<std::mutex> lock(m_mutex);

	if (m_idleThreads < 1)
	{
		// We need to launch a new thread - pass it the initial work to do to avoid queue contention
		std::thread newThread(&Dispatcher::workerThread, this, work);
		newThread.detach();
	}
	else
	{
		m_workQueue.push(work);
		lock.unlock();

		m_workQueueCond.notify_one();
	}
}

void Dispatcher::workerThread(WorkFunction initialWork)
{
	// Do our initial work
	initialWork();

	while(true)
	{
		std::unique_lock<std::mutex> lock(m_mutex);

		// We're now idle
		m_idleThreads++;

		// Wait for work to do for five seconds
		std::chrono::seconds timeout(5);

		if (!m_workQueueCond.wait_for(lock, timeout, [=]{return !m_workQueue.empty();}))
		{
			// Nothing to do; give up our thread
			m_idleThreads--;
			return;
		}

		// Grab the work
		WorkFunction queuedWork(m_workQueue.front());
		m_workQueue.pop();

		// Release the lock
		lock.unlock();

		// Run the queued work outside of the lock
		queuedWork();
	}
}

}
}
