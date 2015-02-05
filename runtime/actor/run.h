#ifndef _LLIBY_ACTOR_RUN_H
#define _LLIBY_ACTOR_RUN_H

#include <memory>

#include "actor/Mailbox.h"
#include "actor/ActorClosureCell.h"

namespace lliby
{
class World;

namespace actor
{
class Message;

/**
 * Initialises a new actor and returns its mailbox
 *
 * This will run the actor's closure procedure in a new world on the current thread. It will then store the initial
 * behaviour and put the new actor to sleep on its mailbox.
 */
std::shared_ptr<Mailbox> run(World &parentWorld, ActorClosureCell *closureCell);

/**
 * Wakes a sleeping actor to handle any queued messages
 *
 * This will dequeue all messages from the mailbox and process it with the actor's current behaviour. Once the mailbox
 * is empty or the actor has been asked to stop the function will return.
 */
void wake(World *actorWorld);

}
}

#endif
