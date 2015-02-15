#include "actor/run.h"

#include "actor/ActorContext.h"
#include "actor/ActorBehaviourCell.h"
#include "actor/cloneCell.h"
#include "actor/PoisonPillCell.h"

#include "alloc/StrongRef.h"

#include "sched/Dispatcher.h"

#include "dynamic/State.h"
#include "dynamic/SchemeException.h"

#include "actor/FailureAction.h"

namespace lliby
{
namespace actor
{

namespace
{
	/**
	 * Handles the failure of an actor
	 *
	 * @return  True if the actor should continue running, false otherwise
	 */
	bool handleActorFailure(World *actorWorld)
	{
		ActorContext *context = actorWorld->actorContext();
		assert(context != nullptr);

		switch(context->selfFailureAction())
		{
		case FailureAction::Resume:
			return true;

		case FailureAction::Restart:
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

		case FailureAction::Stop:
			return false;
		}
	}
}

std::shared_ptr<Mailbox> run(World &parentWorld, ActorClosureCell *closureCell)
{
	// Create a new world to launch
	auto *actorWorld = new World;

	// Clone our closure in to the new world
	dynamic::State *captureState = parentWorld.activeStateCell()->state();
	auto clonedClosureCell = static_cast<ActorClosureCell*>(cloneCell(actorWorld->cellHeap, closureCell, captureState));

	// Determine the actor's failure action
	actor::FailureAction failureAction = parentWorld.childActorFailureAction();

	// Make the world an actor
	ActorContext *context = new ActorContext(clonedClosureCell, failureAction);
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

	// See if we've received any messages; if not put ourselves to sleep
	wake(actorWorld);

	return context->mailbox();
}

void wake(World *actorWorld)
{
	// Pull some useful variables out of our world
	ActorContext *context = actorWorld->actorContext();

	while(true)
	{
		if (context->mailbox()->stopRequested())
		{
			// We should die promptly
			break;
		}

		actor::Message *msg = context->mailbox()->receive(actorWorld);

		if (msg == nullptr)
		{
			// No more messages; go back to sleep
			return;
		}

		if (msg->messageCell() == PoisonPillCell::instance())
		{
			// We got a poison pill!
			delete msg;
			break;
		}

		// Take ownership of the heap
		actorWorld->cellHeap.splice(msg->heap());

		// Grab the root cell
		AnyCell *msgCell = msg->messageCell();

		// Update our sender
		context->setSender(msg->sender());

		// Delete the message
		delete msg;

		try
		{
			actorWorld->run([=] (World &world) {
				context->behaviour()->apply(world, msgCell);
			});
		}
		catch (dynamic::SchemeException &except)
		{
			if (handleActorFailure(actorWorld))
			{
				// Keep going!
				continue;
			}
			else
			{
				// Kill the actor
				break;
			}
		}
	}

	context->mailbox()->setStopped();
	delete actorWorld;
}

}
}
