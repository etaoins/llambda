#ifndef _LLIBY_ACTOR_FAILUREACTION_H
#define _LLIBY_ACTOR_FAILUREACTION_H

#include <cstdint>

namespace lliby
{
namespace actor
{

/**
 * Action to take upon actor failure
 */
enum class FailureAction : std::int32_t
{
	/**
	 * Resumes the actor with its current state on the next message
	 */
	Resume = 0,

	/**
	 * Restarts the actor with a new state
	 */
	Restart = 1,

	/**
	 * Stops the actor permanently
	 */
	Stop = 2
};

}
}

#endif
