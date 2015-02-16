#include "actor/Runner.h"

#include <cassert>

#include "actor/ActorContext.h"
#include "actor/ActorBehaviourCell.h"
#include "actor/cloneCell.h"
#include "actor/PoisonPillCell.h"

#include "alloc/StrongRef.h"

#include "sched/Dispatcher.h"

#include "dynamic/State.h"
#include "dynamic/SchemeException.h"

#include "actor/LifecycleAction.h"

namespace lliby
{
namespace actor
{

namespace
{
	/**
	 * Default supervisor strategy
	 */
	LifecycleAction defaultSupervisorStrategy(AnyCell *errorObject)
	{
		return LifecycleAction::Restart;
	}

	/**
	 * Converts a failure action symbol to a lifecycle action
	 *
	 * This assumes the failure action is defined as (U 'stop 'resume 'restart)
	 */
	LifecycleAction failureActionToLifecycleAction(SymbolCell *failureAction)
	{
		if (failureAction->byteLength() == 4)
		{
			// 'stop
			return LifecycleAction::Stop;
		}
		else if (failureAction->byteLength() == 6)
		{
			// 'resume
			return LifecycleAction::Resume;
		}
		else
		{
			// 'restart
			assert(failureAction->byteLength() == 7);
			return LifecycleAction::Restart;
		}
	}

	/**
	 * Handles the failure of an actor
	 *
	 * Returns if the actor should continue running
	 */
	bool performLifecycleAction(World *actorWorld, LifecycleAction lifecycleAction)
	{
		ActorContext *context = actorWorld->actorContext();
		assert(context != nullptr);

		switch(lifecycleAction)
		{
		case LifecycleAction::Resume:
			return true;

		case LifecycleAction::Restart:
			try
			{
				// Restart the actor
				actorWorld->run([&] (World &world) {
					auto behaviourCell = context->closure()->apply(*actorWorld);
					context->setBehaviour(static_cast<ActorBehaviourCell*>(behaviourCell));
				});

				return true;
			}
			catch (dynamic::SchemeException &except)
			{
				// Threw an exception during restart; give up
				return false;
			}

		case LifecycleAction::Stop:
			return false;
		}
	}
}

std::shared_ptr<Mailbox> Runner::start(World &parentWorld, ActorClosureCell *closureCell)
{
	// Create a new world to launch
	auto *actorWorld = new World;

	// Clone our closure in to the new world
	dynamic::State *captureState = parentWorld.activeStateCell()->state();
	auto clonedClosureCell = static_cast<ActorClosureCell*>(cloneCell(actorWorld->cellHeap, closureCell, captureState));

	// Find our supervisor
	std::weak_ptr<Mailbox> supervisor;

	if (parentWorld.actorContext())
	{
		supervisor = parentWorld.actorContext()->mailbox();
	}

	// Make the world an actor
	ActorContext *context = new ActorContext(clonedClosureCell, supervisor);
	actorWorld->setActorContext(context);

	ActorBehaviourCell *behaviourCell;

	// Initialise the actor's closure in its world but our thread
	actorWorld->run([&] (World &world) {
		behaviourCell = clonedClosureCell->apply(world);
	});

	// Set our initial behaviour
	actorWorld->actorContext()->setBehaviour(behaviourCell);

	// Add us a child actor
	parentWorld.addChildActor(context->mailbox());

	// Wake if we've received any messages; otherwise go to sleep
	context->mailbox()->conditionalQueueWake(actorWorld);

	return context->mailbox();
}

void Runner::wake(World *actorWorld)
{
	// Pull some useful variables out of our world
	ActorContext *context = actorWorld->actorContext();
	const std::shared_ptr<Mailbox> &mailbox = context->mailbox();

	while(true)
	{
		Message *msg;
		LifecycleAction requestedAction;

		Mailbox::ReceiveResult result = mailbox->receive(actorWorld, &msg, &requestedAction);

		if (result == Mailbox::ReceiveResult::WentToSleep)
		{
			// Went to sleep - give up our thread
			return;
		}
		else if (result == Mailbox::ReceiveResult::TookLifecycleAction)
		{
			// Have a lifecycle action
			if (!performLifecycleAction(actorWorld, requestedAction))
			{
				// We were asked to die
				break;
			}

			mailbox->setState(Mailbox::State::Running);
		}
		else
		{
			// Got a message!
			if (msg->messageCell() == PoisonPillCell::instance())
			{
				// We got a poison pill!
				delete msg;
				break;
			}

			// Take ownership of the heap
			actorWorld->cellHeap.splice(msg->heap());

			// Grab the type and root cell
			Message::Type type = msg->type();
			AnyCell *msgCell = msg->messageCell();

			// Update our sender
			context->setSender(msg->sender());

			// Delete the message
			delete msg;

			try
			{
				if (type == Message::Type::SupervisedFailure)
				{
					LifecycleAction lifecycleAction;

					if (context->supervisorStrategy())
					{
						SymbolCell *failureAction;

						// Consult our Scheme supervisor strategy
						actorWorld->run([&] (World &world) {
							failureAction = context->supervisorStrategy()->apply(world, msgCell);
						});

						lifecycleAction = failureActionToLifecycleAction(failureAction);
					}
					else
					{
						// Use the default strategy
						lifecycleAction = defaultSupervisorStrategy(msgCell);
					}

					std::shared_ptr<Mailbox> sender(context->sender().lock());

					if (sender)
					{
						sender->requestLifecycleAction(lifecycleAction);
					}
				}
				else if (type == Message::Type::User)
				{
					actorWorld->run([=] (World &world) {
						context->behaviour()->apply(world, msgCell);
					});
				}
			}
			catch (dynamic::SchemeException &except)
			{
				handleRunningActorException(actorWorld, except);
			}
		}
	}

	mailbox->setState(Mailbox::State::Stopped);
	delete actorWorld;
}

void Runner::handleRunningActorException(World *actorWorld, dynamic::SchemeException &except)
{
	ActorContext *context = actorWorld->actorContext();
	std::shared_ptr<Mailbox> supervisor = context->supervisor().lock();

	// We're failed - this prevent us from being woken up until we get a lifecycle action request
	context->mailbox()->setState(Mailbox::State::Failed);

	if (!supervisor)
	{
		// Request the default action
		LifecycleAction action(defaultSupervisorStrategy(except.object()));

		context->mailbox()->requestLifecycleAction(action);
	}
	else
	{
		// Send our supervisor a message
		actor::Message *msg = Message::createFromCell(except.object(), context->mailbox(), Message::Type::SupervisedFailure);
		supervisor->tell(msg);
	}
}

}
}
