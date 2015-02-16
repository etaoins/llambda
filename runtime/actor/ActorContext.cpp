#include "actor/ActorContext.h"
#include "actor/Mailbox.h"

namespace lliby
{
namespace actor
{

ActorContext::ActorContext(ActorClosureCell *closure, std::weak_ptr<Mailbox> supervisor) :
	m_mailbox(new Mailbox),
	m_closure(closure),
	m_supervisor(supervisor)
{
}


}
}
