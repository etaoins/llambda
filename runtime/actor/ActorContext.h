#ifndef _LLIBY_ACTOR_ACTORCONTEXT_H
#define _LLIBY_ACTOR_ACTORCONTEXT_H

#include <memory>

#include "ActorBehaviourCell.h"

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
	ActorContext();

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

private:
	// This is lazily initialised on first use
	mutable std::shared_ptr<actor::Mailbox> m_mailbox;
	std::weak_ptr<actor::Mailbox> m_sender;

	ActorBehaviourCell *m_behaviour = nullptr;
};


}
}

#endif
