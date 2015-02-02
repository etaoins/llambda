#include "actor/ActorProcedureCell.h"

#include "binding/MailboxCell.h"
#include "binding/UnitCell.h"
#include "binding/TypedProcedureCell.h"

#include "actor/Mailbox.h"
#include "actor/Message.h"
#include "actor/cloneCell.h"

using namespace lliby;

extern "C"
{

using ReceiveProc = TypedProcedureCell<void, AnyCell*>;

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

AnyCell* llactor_ask(World &world, MailboxCell *destMailboxCell, AnyCell *messageCell)
{
	// Create a temporary mailbox
	std::shared_ptr<actor::Mailbox> senderMailbox(new actor::Mailbox());
	std::shared_ptr<actor::Mailbox> destMailbox(destMailboxCell->mailbox().lock());

	try
	{
		if (destMailbox)
		{
			actor::Message *msg = actor::Message::createFromCell(messageCell, senderMailbox);
			destMailbox->send(msg);
		}
	}
	catch(actor::UnclonableCellException &e)
	{
		e.signalSchemeError(world, "(ask)");
	}

	return senderMailbox->receiveInto(world);
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

void llactor_receive(World &world, ReceiveProc *receiveProcRaw)
{
	std::shared_ptr<actor::Mailbox> mailbox(world.mailbox());
	alloc::StrongRef<ReceiveProc> receiveProc(world, receiveProcRaw);

	while(!mailbox->stopRequested())
	{
		AnyCell *msg = mailbox->receiveInto(world);
		receiveProc->apply(world, msg);
	}
}

void llactor_stop(MailboxCell *mailboxCell)
{
	std::shared_ptr<actor::Mailbox> mailbox(mailboxCell->mailbox());

	if (mailbox)
	{
		mailbox->requestStop();
	}
}

bool llactor_mailbox_is_open(World &world, MailboxCell *mailboxCell)
{
	return !mailboxCell->mailbox().expired();
}

}
