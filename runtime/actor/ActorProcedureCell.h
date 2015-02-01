#ifndef _LLIBY_ACTOR_ACTORPROCEDURECELL_H
#define _LLIBY_ACTOR_ACTORPROCEDURECELL_H

#include "binding/TypedProcedureCell.h"

#include <memory>

namespace lliby
{
class World;

namespace actor
{
class Mailbox;

/**
 * Represents a procedure passed to the (start-actor) procedure
 */
class ActorProcedureCell : public TypedProcedureCell<void>
{
public:
	/**
	 * Launches a new world and executes the actor in that world
	 *
	 * @return Mailbox of the newly created actor
	 */
	std::shared_ptr<Mailbox> start(World &parentWorld);
};

}
}

#endif
