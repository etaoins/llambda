#ifndef _LLIBY_ACTOR_ACTORCONTEXT_H
#define _LLIBY_ACTOR_ACTORCONTEXT_H

#include <memory>

#include "actor/ActorClosureCell.h"
#include "actor/ActorBehaviourCell.h"
#include "actor/FailureAction.h"

namespace lliby
{
namespace actor
{
class Mailbox;

/**
 * Context for a running actor
 */
class ActorContext
{
public:
	ActorContext(ActorClosureCell *closure, ActorBehaviourCell *initialBehaviour, FailureAction selfFailureAction);

	/**
	 * Returns the current mailbox for this world
	 */
	const std::shared_ptr<actor::Mailbox>& mailbox() const
	{
		return m_mailbox;
	}

	/**
	 * Returns the mailbox for the last received message in the world
	 */
	const std::weak_ptr<actor::Mailbox>& sender()
	{
		return m_sender;
	}

	/**
	 * Sets the mailbox of rthe last received message in the world
	 */
	void setSender(const std::weak_ptr<actor::Mailbox> &sender)
	{
		m_sender = sender;
	}

	/**
	 * Returns the closure cell used to create the actor
	 *
	 * This is used during restart to re-initialise the actor
	 */
	ActorClosureCell* closure()
	{
		return m_closure;
	}

	/**
	 * Returns a reference to the closure cell
	 *
	 * This is used by the garbage collector
	 */
	ActorClosureCell** closureRef()
	{
		return &m_closure;
	}

	/**
	 * Returns the current behaviour for the actor
	 */
	ActorBehaviourCell* behaviour()
	{
		return m_behaviour;
	}

	/**
	 * Returns a reference to the current behaviour
	 *
	 * This is used by the garbage collector
	 */
	ActorBehaviourCell** behaviourRef()
	{
		return &m_behaviour;
	}

	/**
	 * Returns the current behaviour for the actor
	 */
	void setBehaviour(ActorBehaviourCell *behaviour)
	{
		m_behaviour = behaviour;
	}

	/**
	 * Sets the failure action for handling our own failures
	 *
	 * This is inherited from our parent's childFailureAction()
	 */
	FailureAction selfFailureAction() const
	{
		return m_selfFailureAction;
	}

private:
	// This is lazily initialised on first use
	mutable std::shared_ptr<actor::Mailbox> m_mailbox;
	std::weak_ptr<actor::Mailbox> m_sender;

	ActorClosureCell *m_closure = nullptr;
	ActorBehaviourCell *m_behaviour = nullptr;

	FailureAction m_selfFailureAction;
};

}
}

#endif
