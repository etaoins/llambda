#include "actor/run.h"

#include "actor/ActorContext.h"
#include "actor/ActorBehaviourCell.h"
#include "actor/cloneCell.h"

#include "sched/Dispatcher.h"

#include "dynamic/State.h"
#include "dynamic/SchemeException.h"

namespace lliby
{
namespace actor
{

std::shared_ptr<Mailbox> run(World &parentWorld, ActorClosureCell *closureCell)
{
	// Create a new world to launch
	auto *actorWorld = new World;

	// Clone ourselves and our closure in to the new world
	dynamic::State *captureState = parentWorld.activeStateCell()->state();
	auto clonedSelf = static_cast<ActorClosureCell*>(cloneCell(actorWorld->cellHeap, closureCell, captureState));

	ActorBehaviourCell *behaviourCell;

	// Initialise the actor's closure in its world but our thread
	actorWorld->run([&] (World &world) {
		behaviourCell = clonedSelf->apply(world);
	});

	// Make the world an actor
	ActorContext *context = actorWorld->createActorContext();
	context->setBehaviour(behaviourCell);

	// Set our initial behaviour
	actorWorld->actorContext()->setBehaviour(behaviourCell);

	// Add us a child actor
	parentWorld.addChildActor(context->mailbox());

	// Go to sleep on receive
	context->mailbox()->sleepActor(actorWorld);

	return context->mailbox();
}

void wake(World *actorWorld)
{
	try
	{
		// Pull some useful variables out of our world
		ActorContext *context = actorWorld->actorContext();
		std::shared_ptr<Mailbox> mailbox(context->mailbox());

		while(true)
		{
			if (mailbox->stopRequested())
			{
				// We should die promptly
				break;
			}

			actor::Message *msg = mailbox->receive();

			if (msg == nullptr)
			{
				// No more messages; go back to sleep
				mailbox->sleepActor(actorWorld);
				return;
			}

			// Take ownership of the heap
			actorWorld->cellHeap.splice(msg->heap());

			// Grab the root cell
			AnyCell *msgCell = msg->messageCell();

			// Update our sender
			context->setSender(msg->sender());

			// Delete the message
			delete msg;

			actorWorld->run([=] (World &world) {
				context->behaviour()->apply(world, msgCell);
			});
		}
	}
	catch (dynamic::SchemeException &except)
	{
		// XXX: Support supervision
	}

	actorWorld->actorContext()->mailbox()->stopped();
	delete actorWorld;
}

}
}
