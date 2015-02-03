#include "actor/ActorContext.h"
#include "actor/Mailbox.h"

namespace lliby
{
namespace actor
{

ActorContext::ActorContext() :
	m_mailbox(new Mailbox)
{
}


}
}
