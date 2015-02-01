#include "actor/Message.h"
#include "actor/cloneCell.h"
#include "alloc/Finalizer.h"

namespace lliby
{
namespace actor
{

Message* Message::createFromCell(AnyCell *cell, const std::shared_ptr<Mailbox> &sender)
{
	Message *msg = new Message;

	// Clone in to the message's heap
	msg->m_messageCell = cloneCell(msg->m_heap, cell);
	// Set the sender
	msg->m_sender = sender;

	return msg;
}


}
}
