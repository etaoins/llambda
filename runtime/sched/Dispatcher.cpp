#include "sched/Dispatcher.h"

#include <thread>
#include <chrono>

namespace lliby
{
namespace sched
{

namespace
{
	Dispatcher DefaultInstance;
}

Dispatcher::Dispatcher() :
	m_idleThreads(0),
	m_runningThreads(0)
{
}

Dispatcher::~Dispatcher()
{
	waitForDrain();
}

Dispatcher& Dispatcher::defaultInstance()
{
	return DefaultInstance;
}

void Dispatcher::dispatch(const WorkFunction &work)
{
	std::unique_lock<std::mutex> lock(m_mutex);

	if (m_idleThreads < 1)
	{
		m_runningThreads++;
		lock.unlock();

		try
		{
			// We need to launch a new thread - pass it the initial work to do to avoid queue contention
			std::thread newThread(&Dispatcher::workerThread, this, work);
			newThread.detach();

			return;
		}
		catch(std::system_error &)
		{
			// Failed to launch a thread. This can happen on low resource situations. Fall back to queuing
			lock.lock();
			m_runningThreads--;

			m_drainCond.notify_all();
		}
	}

	m_workQueue.push(work);
	lock.unlock();

	m_workQueueCond.notify_one();
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

		// Wait until we we either timeout or start draining
		std::chrono::seconds timeout(5);
		m_workQueueCond.wait_for(lock, timeout, [=]{return m_draining || !m_workQueue.empty();});

		// We're no longer idle
		m_idleThreads--;

		if (m_workQueue.empty())
		{
			// Nothing to do; give up our thread
			m_runningThreads--;
			m_drainCond.notify_all();

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

void Dispatcher::waitForDrain()
{
	std::unique_lock<std::mutex> lock(m_mutex);

	// Signal that we want to drain
	m_draining = true;

	// Wake up all of our worker threads so they notice we're draining
	m_workQueueCond.notify_all();

	// Wait for all of the threads to stop
	m_drainCond.wait(lock, [=]{
		return m_workQueue.empty() && m_runningThreads == 0;
	});

	// We're no longer draining
	m_draining = false;
}

}
}
