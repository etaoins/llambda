#ifndef _LLIBY_ACTOR_MAILBOX_H
#define _LLIBY_ACTOR_MAILBOX_H

#include "binding/AnyCell.h"
#include "actor/Message.h"

#include <mutex>
#include <queue>
#include <condition_variable>
#include <atomic>

namespace lliby
{
class World;

namespace actor
{
class Message;

/**
 * Mailbox for an actor
 *
 * This contains a thread-safe queue of messages for the actor
 */
class Mailbox
{
public:
	~Mailbox();

	/**
	 * Pushes a message on the mailbox's message queue
	 *
	 * This is asynchronous; it will return as soon as the message is successfully enqueued. The mailbox takes
	 * ownership of the message.
	 */
	void send(Message *);

	/**
	 * Pops a message from the message queue
	 *
	 * This is blocking. If the mailbox is empty then this method will block indefinitely until one is received. The
	 * mailbox passes ownership of the message to its caller.
	 */
	Message *receive();

	/**
	 * Receives a message in to the passed world
	 *
	 * This internally calls receive(), splices the message's heap in to the world's heap, frees the message and returns
	 * to root message cell
	 */
	AnyCell *receiveInto(World &world);

	/**
	 * Sets a flag indicating if the owner of this mailbox should stop
	 */
	void requestStop();

	/**
	 * Returns true if the owner of this mailbox have been asked to stop
	 */
	bool stopRequested() const;

private:
	std::mutex m_messageQueueMutex;
	std::condition_variable m_messageQueueCond;
	std::queue<Message*> m_messageQueue;

	std::atomic<bool> m_stopRequested;
};

}
}

#endif
