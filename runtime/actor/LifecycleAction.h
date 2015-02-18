#ifndef _LLIBY_ACTOR_LIFECYCLEACTION_H
#define _LLIBY_ACTOR_LIFECYCLEACTION_H

#include <cstdint>

namespace lliby
{
namespace actor
{

/**
 * Action to take upon actor failure
 *
 * The numeric values of these actions correspond to their priority - e.g. stop actions have the highest priority. If a
 * lifecycle action is requested with a lower priority than the current action it will be ignored.
 */
enum class LifecycleAction : std::int32_t
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
