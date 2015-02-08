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
	Mailbox();
	~Mailbox();

	/**
	 * Pushes a message on the mailbox's message queue
	 *
	 * This is asynchronous; it will return as soon as the message is successfully enqueued. The mailbox takes
	 * ownership of the message.
	 */
	void send(Message *);

	/**
	 * Attempts to pop message from the message queue
	 *
	 * This is non-blocking. If the message box is empty then nullptr is returned. The mailbox passes ownership of the
	 * message to its caller.
	 */
	Message *receive();

	/**
	 * Asks the mailbox for a synchronous response
	 *
	 * This internally creates a temporary mailbox and sends a message from there. If the actor is currently sleeping
	 * it will be woken synchronously in the current thread.
	 */
	AnyCell *ask(World &world, AnyCell *requestCell);

	/**
	 * Sets a flag indicating if the owner of this mailbox should stop
	 */
	void requestStop();

	/**
	 * Returns true if the owner of this mailbox have been asked to stop
	 */
	bool stopRequested() const;

	/**
	 * Indicates that this actor is stopped
	 */
	void stopped();

	/**
	 * Waits until this actor has stopped
	 */
	void waitForStop();

	/**
	 * Marks the passed actor World as sleeping on this mailbox
	 */
	void sleepActor(World *sleepingReceiver);

private:
	std::mutex m_mutex;
	std::condition_variable m_messageQueueCond;
	std::queue<Message*> m_messageQueue;
	World *m_sleepingReceiver = nullptr;

	std::atomic<bool> m_stopRequested;
	std::condition_variable m_stoppedCond;
	bool m_stopped = false;
};

}
}

#endif
