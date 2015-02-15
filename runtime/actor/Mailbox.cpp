#include "actor/Mailbox.h"

#include <chrono>

#include "core/World.h"
#include "sched/Dispatcher.h"
#include "actor/run.h"

namespace lliby
{
namespace actor
{

namespace
{
#ifdef _LLIBY_CHECK_LEAKS
	std::atomic<std::size_t> allocationCount(0);
#endif
}

Mailbox::Mailbox() :
	m_stopRequested(false)
{
#ifdef _LLIBY_CHECK_LEAKS
	allocationCount++;
#endif
}

Mailbox::~Mailbox()
{
	// Free all of our messages. We don't need a lock here - if this isn't being called from the last reference we're
	// in trouble
	while(!m_messageQueue.empty())
	{
		delete m_messageQueue.front();
		m_messageQueue.pop();
	}

#ifdef _LLIBY_CHECK_LEAKS
	allocationCount--;
#endif
}

void Mailbox::send(Message *message)
{
	// Add to the queue
	{
		std::unique_lock<std::mutex> lock(m_mutex);
		m_messageQueue.push(message);

		if (m_sleepingReceiver)
		{
			// We're waking the world; null out the sleeper
			World *toWake = m_sleepingReceiver;
			m_sleepingReceiver = nullptr;

			lock.unlock();

			sched::Dispatcher::defaultInstance().dispatch([=] {
				wake(toWake);
			});
		}
		else
		{
			lock.unlock();

			// Notify
			m_messageQueueCond.notify_one();
		}
	}

}

Message* Mailbox::receive(World *sleepingReceiver)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	if (m_messageQueue.empty())
	{
		assert(m_sleepingReceiver == nullptr);
		assert(sleepingReceiver->actorContext());

		m_sleepingReceiver = sleepingReceiver;
		return nullptr;
	}

	Message *msg = m_messageQueue.front();
	m_messageQueue.pop();

	return msg;
}

AnyCell* Mailbox::ask(World &world, AnyCell *requestCell, std::int64_t timeoutUsecs)
{
	if (stopRequested())
	{
		// We'll never get this message - timeout right away
		return nullptr;
	}

	// Create a temporary mailbox
	std::shared_ptr<actor::Mailbox> senderMailbox(new actor::Mailbox());

	// Create the request
	actor::Message *request = actor::Message::createFromCell(requestCell, senderMailbox);

	// Send the request
	{
		std::unique_lock<std::mutex> receiverLock(m_mutex);

		m_messageQueue.push(request);

		if (m_sleepingReceiver)
		{
			World *toWake = m_sleepingReceiver;
			m_sleepingReceiver = nullptr;

			receiverLock.unlock();

			// Synchronously wake our receiver in the hopes that it will reply immediately
			wake(toWake);
		}
		else
		{
			receiverLock.unlock();
			m_messageQueueCond.notify_one();
		}
	}

	// Block on the sender mailbox for a reply
	{
		std::unique_lock<std::mutex> senderLock(senderMailbox->m_mutex);

		// Wait for the sender mailbox to be non-empty
		const std::chrono::microseconds timeout(timeoutUsecs);
		bool hasMessage = senderMailbox->m_messageQueueCond.wait_for(senderLock, timeout, [=] {
			return !senderMailbox->m_messageQueue.empty();
		});

		if (!hasMessage)
		{
			// We timed out
			return nullptr;
		}

		// Get the message
		Message *reply = senderMailbox->m_messageQueue.front();
		senderMailbox->m_messageQueue.pop();

		// Release the lock
		senderLock.unlock();

		// Take ownership of the heap
		world.cellHeap.splice(reply->heap());

		// Grab the root cell and delete the message
		AnyCell *msgCell = reply->messageCell();
		delete reply;

		return msgCell;
	}
}

void Mailbox::requestStop()
{
	std::unique_lock<std::mutex> lock(m_mutex);
	m_stopRequested = true;

	if (m_sleepingReceiver)
	{
		World *toWake = m_sleepingReceiver;
		m_sleepingReceiver = nullptr;

		lock.unlock();

		// Synchronously wake our receiver in the hopes that it will reply immediately
		wake(toWake);
	}
	else
	{
		lock.unlock();
		m_messageQueueCond.notify_one();
	}
}

bool Mailbox::stopRequested() const
{
	return m_stopRequested;
}

void Mailbox::setStopped()
{
	{
		std::lock_guard<std::mutex> lock(m_mutex);
		m_stopped = true;
	}

	m_stoppedCond.notify_all();
}

void Mailbox::waitForStop()
{
	std::unique_lock<std::mutex> lock(m_mutex);
	m_stoppedCond.wait(lock, [=]{return m_stopped;});
}

void Mailbox::sleepActor(World *sleepingReceiver)
{
	std::lock_guard<std::mutex> guard(m_mutex);

	assert(m_sleepingReceiver == nullptr);
	assert(sleepingReceiver->actorContext());

	m_sleepingReceiver = sleepingReceiver;
}

#ifdef _LLIBY_CHECK_LEAKS

std::size_t Mailbox::instanceCount()
{
	return allocationCount.load(std::memory_order_relaxed);
}

#else

std::size_t Mailbox::instanceCount()
{
	return 0;
}

#endif

}
}
