#include "actor/Mailbox.h"

#include "core/World.h"

namespace lliby
{
namespace actor
{

Mailbox::~Mailbox()
{
	// Free all of our messages. We don't need a lock here - if this isn't being called from the last reference we're
	// in trouble
	while(!m_messageQueue.empty())
	{
		delete m_messageQueue.front();
		m_messageQueue.pop();
	}
}

void Mailbox::send(Message *message)
{
	// Add to the queue
	{
		std::lock_guard<std::mutex> guard(m_messageQueueMutex);
		m_messageQueue.push(message);
	}

	// Notify
	m_messageQueueCond.notify_one();
}

Message* Mailbox::receive()
{
	std::unique_lock<std::mutex> lock(m_messageQueueMutex);

	// Wait for the mailbox to be non-empty
	m_messageQueueCond.wait(lock, [=]{return !m_messageQueue.empty();});

	// Get the message
	Message *message = m_messageQueue.front();
	m_messageQueue.pop();

	// Release the lock
	lock.unlock();

	return message;
}

AnyCell* Mailbox::receiveInto(World &world)
{
	Message *msg = receive();

	// Take ownership of the heap
	world.cellHeap.splice(msg->heap());

	// Grab the root cell and delete the message
	AnyCell *msgCell = msg->messageCell();
	delete msg;

	return msgCell;
}

void Mailbox::requestStop()
{
	m_stopRequested = true;
}

bool Mailbox::stopRequested() const
{
	return m_stopRequested;
}

}
}
