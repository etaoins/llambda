#include "actor/ActorContext.h"
#include "actor/Mailbox.h"

namespace lliby
{
namespace actor
{

ActorContext::ActorContext(ActorClosureCell *closure, FailureAction selfFailureAction) :
	m_mailbox(new Mailbox),
	m_closure(closure),
	m_selfFailureAction(selfFailureAction)
{
}


}
}
