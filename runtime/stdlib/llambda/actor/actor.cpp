#include "actor/ActorProcedureCell.h"

#include "binding/MailboxCell.h"
#include "binding/UnitCell.h"
#include "actor/Mailbox.h"
#include "actor/Message.h"
#include "actor/cloneCell.h"

using namespace lliby;

extern "C"
{

MailboxCell* llactor_act(World &world, actor::ActorProcedureCell *actorProc)
{
	try
	{
		return MailboxCell::createInstance(world, actorProc->start(world));
	}
	catch(actor::UnclonableCellException &e)
	{
		e.signalSchemeError(world, "(act)");
	}
}

void llactor_send(World &world, MailboxCell *destMailboxCell, AnyCell *messageCell)
{
	std::shared_ptr<actor::Mailbox> destMailbox(destMailboxCell->mailbox().lock());

	if (!destMailbox)
	{
		// Destination has gone away
		return;
	}

	try
	{
		actor::Message *msg = actor::Message::createFromCell(messageCell, world.mailbox());
		destMailbox->send(msg);
	}
	catch(actor::UnclonableCellException &e)
	{
		e.signalSchemeError(world, "(!)");
	}
}

MailboxCell *llactor_self(World &world)
{
	return MailboxCell::createInstance(world, world.mailbox());
}

AnyCell *llactor_sender(World &world)
{
	std::shared_ptr<actor::Mailbox> mailbox(world.sender());

	if (!mailbox)
	{
		// No last sender
		return UnitCell::instance();
	}

	return MailboxCell::createInstance(world, mailbox);
}

AnyCell* llactor_receive(World &world)
{
	return world.mailbox()->receiveInto(world);
}

bool llactor_mailbox_is_open(World &world, MailboxCell *mailboxCell)
{
	return !mailboxCell->mailbox().expired();
}

}
