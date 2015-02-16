#include "actor/Message.h"
#include "actor/cloneCell.h"
#include "alloc/Finalizer.h"

namespace lliby
{
namespace actor
{

Message* Message::createFromCell(AnyCell *cell, const std::shared_ptr<Mailbox> &sender, Type type)
{
	Message *msg = new Message;

	msg->m_type = type;

	// Clone in to the message's heap. Don't give a capture state; we shouldn't be passing parameter procedures via (!)
	msg->m_messageCell = cloneCell(msg->m_heap, cell, nullptr);

	// Set the sender
	msg->m_sender = sender;

	return msg;
}


}
}
