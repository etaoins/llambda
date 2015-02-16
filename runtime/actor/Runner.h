#ifndef _LLIBY_ACTOR_RUNNER_H
#define _LLIBY_ACTOR_RUNNER_H

#include <memory>

#include "actor/Mailbox.h"
#include "actor/ActorClosureCell.h"

namespace lliby
{
class World;

namespace dynamic
{
class SchemeException;
}

namespace actor
{
class Message;

class Runner
{
public:
	/**
	 * Initialises a new actor and returns its mailbox
	 *
	 * This will run the actor's closure procedure in a new world on the current thread. It will then store the initial
	 * behaviour and put the new actor to sleep on its mailbox.
	 */
	static std::shared_ptr<Mailbox> start(World &parentWorld, ActorClosureCell *closureCell);

	/**
	 * Wakes a sleeping actor to handle any queued messages
	 *
	 * This will dequeue all messages from the mailbox and process it with the actor's current behaviour. Once the mailbox
	 * is empty or the actor has been asked to stop the function will return.
	 */
	static void wake(World *actorWorld);

private:
	/**
	 * Handles an exception thrown by an actor during behaviour message handling
	 */
	static void handleRunningActorException(World *actorWorld, dynamic::SchemeException &except);
};

}
}

#endif
