#include "actor/ActorClosureCell.h"

#include "binding/MailboxCell.h"
#include "binding/UnitCell.h"
#include "binding/TypedProcedureCell.h"

#include "actor/PoisonPillCell.h"
#include "actor/ActorContext.h"
#include "actor/ActorBehaviourCell.h"
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

AnyCell* llactor_ask(World &world, MailboxCell *destMailboxCell, AnyCell *messageCell, std::int64_t timeoutUsecs)
{
	std::shared_ptr<actor::Mailbox> destMailbox(destMailboxCell->mailbox().lock());

	if (!destMailbox)
	{
		signalError(world, ErrorCategory::AskTimeout, "(ask) on closed mailbox");
	}
	else
	{
		try
		{
			AnyCell *result = destMailbox->ask(world, messageCell, timeoutUsecs);

			if (result == nullptr)
			{
				signalError(world, ErrorCategory::AskTimeout, "(ask) timeout");
			}

			return result;
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

	std::shared_ptr<actor::Mailbox> mailbox = context->sender().lock();

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

bool llactor_graceful_stop(MailboxCell *mailboxCell)
{
	std::shared_ptr<actor::Mailbox> mailbox(mailboxCell->mailbox());

	// If there's no mailbox we're already stopped
	if (mailbox)
	{
		mailbox->requestStop();
		mailbox->waitForStop();
	}

	// XXX: Timeout support
	return true;
}

bool llactor_mailbox_is_open(World &world, MailboxCell *mailboxCell)
{
	return !mailboxCell->mailbox().expired();
}

std::int32_t llactor_child_failure_action(World &world)
{
	return static_cast<std::int32_t>(world.childActorFailureAction());
}

void llactor_set_child_failure_action(World &world, std::int32_t failureAction)
{
	world.setChildActorFailureAction(static_cast<actor::FailureAction>(failureAction));
}

actor::PoisonPillCell* llactor_poison_pill_object()
{
	return actor::PoisonPillCell::instance();
}

bool llactor_is_poison_pill_object(AnyCell *cell)
{
	return actor::PoisonPillCell::instance() == cell;
}

void llactor_become(World &world, actor::ActorBehaviourCell *newBehaviour)
{
	actor::ActorContext *context = world.actorContext();

	if (context == nullptr)
	{
		signalError(world, ErrorCategory::NoActor, "Attempted (become) outside actor context");
	}

	context->setBehaviour(newBehaviour);
}

}
