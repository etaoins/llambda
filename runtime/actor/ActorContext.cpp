#include "actor/ActorContext.h"
#include "actor/Mailbox.h"

namespace lliby
{
namespace actor
{

ActorContext::ActorContext(ActorClosureCell *closure, ActorBehaviourCell *initialBehaviour, FailureAction selfFailureAction) :
	m_mailbox(new Mailbox),
	m_closure(closure),
	m_behaviour(initialBehaviour),
	m_selfFailureAction(selfFailureAction)
{
}


}
}
