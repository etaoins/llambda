#include "actor/ActorClosureCell.h"

#include "binding/MailboxCell.h"
#include "binding/UnitCell.h"
#include "binding/TypedProcedureCell.h"

#include "actor/ActorContext.h"
#include "actor/Mailbox.h"
#include "actor/Message.h"
#include "actor/cloneCell.h"
#include "actor/run.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

using ReceiveProc = TypedProcedureCell<void, AnyCell*>;

MailboxCell* llactor_act(World &world, actor::ActorClosureCell *closureProc)
{
	try
	{
		return MailboxCell::createInstance(world, actor::run(world, closureProc));
	}
	catch(actor::UnclonableCellException &e)
	{
		e.signalSchemeError(world, "(act)");
	}
}

void llactor_tell(World &world, MailboxCell *destMailboxCell, AnyCell *messageCell)
{
	std::shared_ptr<actor::Mailbox> destMailbox(destMailboxCell->mailbox().lock());

	if (!destMailbox)
	{
		// Destination has gone away
		return;
	}

	std::shared_ptr<actor::Mailbox> senderMailbox;
	actor::ActorContext *context = world.actorContext();

	// If we don't have an actor context leave the sender mailbox unset. This will eat all messages and return #f for
	// (mailbox-open?)
	if (context)
	{
		senderMailbox = context->mailbox();
	}

	try
	{
		actor::Message *msg = actor::Message::createFromCell(messageCell, senderMailbox);
		destMailbox->send(msg);
	}
	catch(actor::UnclonableCellException &e)
	{
		e.signalSchemeError(world, "(!)");
	}
}

AnyCell* llactor_ask(World &world, MailboxCell *destMailboxCell, AnyCell *messageCell)
{
	std::shared_ptr<actor::Mailbox> destMailbox(destMailboxCell->mailbox().lock());

	if (!destMailbox)
	{
		// XXX: Timeout
		while(true);
	}
	else
	{
		try
		{
			return destMailbox->ask(world, messageCell);
		}
		catch(actor::UnclonableCellException &e)
		{
			e.signalSchemeError(world, "(ask)");
		}
	}
}

MailboxCell *llactor_self(World &world)
{
	actor::ActorContext *context = world.actorContext();

	if (context == nullptr)
	{
		signalError(world, ErrorCategory::NoActor, "Attempted (self) outside actor context");
	}

	return MailboxCell::createInstance(world, context->mailbox());
}

AnyCell *llactor_sender(World &world)
{
	actor::ActorContext *context = world.actorContext();

	if (context == nullptr)
	{
		signalError(world, ErrorCategory::NoActor, "Attempted (sender) outside actor context");
	}

	std::shared_ptr<actor::Mailbox> mailbox(context->sender());

	if (!mailbox)
	{
		// No last sender
		return UnitCell::instance();
	}

	return MailboxCell::createInstance(world, mailbox);
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
