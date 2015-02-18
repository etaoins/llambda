#include "actor/Mailbox.h"

#include <chrono>

#include "core/World.h"
#include "sched/Dispatcher.h"
#include "actor/Runner.h"

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
	m_requestedLifecycleAction(LifecycleAction::Resume)
{
#ifdef _LLIBY_CHECK_LEAKS
	allocationCount++;
#endif
}

Mailbox::~Mailbox()
{
	// Free all of our messages. We don't need a lock here - if this isn't being called from the last reference we're
	// in trouble
	for(auto msg : m_messageQueue)
	{
		delete msg;
	}

#ifdef _LLIBY_CHECK_LEAKS
	allocationCount--;
#endif
}

void Mailbox::tell(Message *message)
{
	// Add to the queue
	std::unique_lock<std::mutex> lock(m_mutex);
	m_messageQueue.push_back(message);

	if (m_sleepingReceiver && (m_state == State::Running))
	{
		// We're waking the world; null out the sleeper
		World *toWake = m_sleepingReceiver;
		m_sleepingReceiver = nullptr;

		lock.unlock();

		sched::Dispatcher::defaultInstance().dispatch([=] {
			Runner::wake(toWake);
		});
	}
	else
	{
		lock.unlock();

		// Notify
		m_messageQueueCond.notify_one();
	}
}

void Mailbox::conditionalQueueWake(World *receiver)
{
	std::unique_lock<std::mutex> lock(m_mutex);

	if (m_lifecycleActionRequested || (!m_messageQueue.empty() && (m_state == State::Running)))
	{
		lock.unlock();

		sched::Dispatcher::defaultInstance().dispatch([=] {
			Runner::wake(receiver);
		});
	}
	else
	{
		m_sleepingReceiver = receiver;
	}
}

Mailbox::ReceiveResult Mailbox::receive(World *sleepingReceiver, Message **msg, LifecycleAction *action)
{
	std::lock_guard<std::mutex> lock(m_mutex);

	if (m_lifecycleActionRequested)
	{
		*action = m_requestedLifecycleAction;
		m_lifecycleActionRequested = false;

		return ReceiveResult::TookLifecycleAction;
	}
	else if ((m_state == State::Running) && !m_messageQueue.empty())
	{
		*msg = m_messageQueue.front();
		m_messageQueue.pop_front();

		return ReceiveResult::PoppedMessage;
	}

	assert(m_sleepingReceiver == nullptr);
	assert(sleepingReceiver->actorContext());

	m_sleepingReceiver = sleepingReceiver;
	return ReceiveResult::WentToSleep;
}

AnyCell* Mailbox::ask(World &world, AnyCell *requestCell, std::int64_t timeoutUsecs)
{
	// Create a temporary mailbox
	std::shared_ptr<actor::Mailbox> senderMailbox(std::make_shared<actor::Mailbox>());

	// Create the request
	actor::Message *request = actor::Message::createFromCell(requestCell, senderMailbox);

	// Send the request
	{
		std::unique_lock<std::mutex> receiverLock(m_mutex);

		m_messageQueue.push_back(request);

		if (m_sleepingReceiver && (m_state == State::Running))
		{
			World *toWake = m_sleepingReceiver;
			m_sleepingReceiver = nullptr;

			receiverLock.unlock();

			// Synchronously wake our receiver in the hopes that it will reply immediately
			Runner::wake(toWake);
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
		senderMailbox->m_messageQueue.pop_front();

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

void Mailbox::requestLifecycleAction(LifecycleAction action)
{
	std::unique_lock<std::mutex> lock(m_mutex);

	if (m_lifecycleActionRequested && (action <= m_requestedLifecycleAction))
	{
		// Nothing to do
		return;
	}

	m_lifecycleActionRequested = true;
	m_requestedLifecycleAction = action;

	if (m_sleepingReceiver)
	{
		World *toWake = m_sleepingReceiver;
		m_sleepingReceiver = nullptr;

		lock.unlock();

		// Synchronously wake our receiver in the hopes that it will reply immediately
		Runner::wake(toWake);
	}
	else
	{
		lock.unlock();
		m_messageQueueCond.notify_one();
	}
}

void Mailbox::setState(State state)
{
	{
		std::lock_guard<std::mutex> lock(m_mutex);
		m_state = state;
	}

	m_stateCond.notify_all();
}

void Mailbox::waitForStop()
{
	std::unique_lock<std::mutex> lock(m_mutex);
	m_stateCond.wait(lock, [=]{return m_state == State::Stopped;});
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
