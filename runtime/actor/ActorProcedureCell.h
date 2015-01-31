#ifndef _LLIBY_ACTOR_ACTORPROCEDURECELL_H
#define _LLIBY_ACTOR_ACTORPROCEDURECELL_H

#include "binding/TypedProcedureCell.h"

namespace lliby
{
namespace actor
{

/**
 * Represents a procedure passed to the (start-actor) procedure
 */
class ActorProcedureCell : public TypedProcedureCell<void>
{
public:
	/**
	 * Launches a new world and executes the actor in that world
	 */
	void start();
};

}
}

#endif
