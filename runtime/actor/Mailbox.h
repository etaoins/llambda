#ifndef _LLIBY_ACTOR_MAILBOX_H
#define _LLIBY_ACTOR_MAILBOX_H

#include "binding/AnyCell.h"
#include "actor/Message.h"
#include "actor/LifecycleAction.h"

#include <cstdint>
#include <mutex>
#include <queue>
#include <condition_variable>

namespace lliby
{
class World;

namespace actor
{
class Message;
class Runner;

/**
 * Mailbox for an actor
 *
 * This contains a thread-safe queue of messages for the actor
 */
class Mailbox
{
	friend class Runner;
public:
	/**
	 * State of the mailbox
	 */
	enum class State
	{
		/**
		 * Mailbox is running and accepting messages
		 */
		Running,

		/**
		 * Mailbox has failed and is awaiting supervisor action
		 */
		Failed,

		/**
		 * Mailbox has permanently stopped
		 */
		Stopped
	};

	Mailbox();
	~Mailbox();

	/**
	 * Pushes a message on the mailbox's message queue
	 *
	 * This is asynchronous; it will return as soon as the message is successfully enqueued. The mailbox takes
	 * ownership of the message.
	 */
	void tell(Message *);

	/**
	 * Asks the mailbox for a synchronous response
	 *
	 * This internally creates a temporary mailbox and sends a message from there. If the actor is currently sleeping
	 * it will be woken synchronously in the current thread.
	 *
	 * @param  world         World to receive the response in
	 * @param  requestCell   Message cell for the initial request
	 * @param  timeoutUsecs  Ask timeout in microseconds
	 * @return Response cell in the passed world or nullptr if the timeout was reached
	 */
	AnyCell *ask(World &world, AnyCell *requestCell, std::int64_t timeoutUsecs);

	/**
	 * Request the actor peforms the specified lifecycle action
	 *
	 * This is used both for supervision and for stopping
	 */
	void requestLifecycleAction(LifecycleAction action);

	/**
	 * Waits until this actor has stopped
	 */
	void waitForStop();

	/**
	 * Returns the current number of live mailbox instances
	 *
	 * This always returns 0 unless _LLIBY_CHECK_LEAKS is defined
	 */
	static std::size_t instanceCount();

// Runner API
protected:
	enum class ReceiveResult
	{
		TookLifecycleAction,
		PoppedMessage,
		WentToSleep
	};

	/**
	 * Attempts to pop message from the message queue or take a lifecycle action
	 *
	 * This is non-blocking. If the message box is empty then nullptr is returned and the passed World is put to sleep
	 * on the mailbox.
	 *
	 * @param  sleepingReceiver  World to put to sleep if there's nothing to receive
	 * @param  msg               Out pointer to message if PoppedMessage is returned. The mailbox passes ownership to
	 *                           the caller
	 * @param  action            Out pointer to the lifecycle action if TookLifecycleAction is returned
	 */
	ReceiveResult receive(World *sleepingReceiver, Message **msg, LifecycleAction *action);

	/**
	 * Sets the current state of the actor
	 */
	void setState(State state);

	/**
	 * Puts us to sleep if there are no messages in the mailbox; otherwise it will wake us asynchronously
	 *
	 * @param  receiver  world to either wake or put to sleep
	 */
	void conditionalQueueWake(World *receiver);

private:
	std::mutex m_mutex;

	std::condition_variable m_messageQueueCond;
	std::queue<Message*> m_messageQueue;
	World *m_sleepingReceiver = nullptr;

	bool m_lifecycleActionRequested = false;
	LifecycleAction m_requestedLifecycleAction;

	std::condition_variable m_stateCond;
	State m_state = State::Running;
};

}
}

#endif
