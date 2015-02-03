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

void llactor_send(World &world, MailboxCell *destMailboxCell, AnyCell *messageCell)
{
	std::shared_ptr<actor::Mailbox> destMailbox(destMailboxCell->mailbox().lock());

	if (!destMailbox)
	{
		// Destination has gone away
		return;
	}

	actor::ActorContext *context = world.actorContext();

	if (context == nullptr)
	{
		signalError(world, ErrorCategory::NoActor, "Attempted (!) outside actor context");
	}

	try
	{
		actor::Message *msg = actor::Message::createFromCell(messageCell, context->mailbox());
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
