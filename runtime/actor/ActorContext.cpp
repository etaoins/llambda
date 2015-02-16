#include "actor/ActorContext.h"
#include "actor/Mailbox.h"

namespace lliby
{
namespace actor
{

ActorContext::ActorContext(ActorClosureCell *closure, const std::weak_ptr<Mailbox> &supervisor) :
	m_mailbox(std::make_shared<Mailbox>()),
	m_closure(closure),
	m_supervisor(supervisor)
{
}


}
}
