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

namespace
{
	void receiveLoop(World *actorWorld, ActorBehaviourCell *behaviourRaw)
	{
		// GC root our behaviour
		alloc::StrongRef<ActorBehaviourCell> behaviour(*actorWorld, behaviourRaw);

		try
		{
			// Pull some useful variables out of our world
			ActorContext *context = actorWorld->actorContext();
			std::shared_ptr<Mailbox> mailbox(context->mailbox());

			while(!mailbox->stopRequested())
			{
				actor::Message *msg = mailbox->receive();

				// Take ownership of the heap
				actorWorld->cellHeap.splice(msg->heap());

				// Grab the root cell and delete the message
				AnyCell *msgCell = msg->messageCell();
				delete msg;

				// Update our sender
				context->setSender(msg->sender());

				behaviour->apply(*actorWorld, msgCell);
			}
		}
		catch (dynamic::SchemeException &except)
		{
			// XXX: Support supervision
		}

		delete actorWorld;
	}
}

std::shared_ptr<Mailbox> run(World &parentWorld, ActorClosureCell *closureCell)
{
	// Create a new world to launch
	auto *actorWorld = new World;

	// Make the world an actor
	actorWorld->createActorContext();

	// Make sure we grab a reference to the actor's mailbox before we start the thread so we don't race
	std::shared_ptr<Mailbox> actorMailbox(actorWorld->actorContext()->mailbox());

	// Clone ourselves and our closure in to the new world
	dynamic::State *captureState = parentWorld.activeStateCell()->state();
	auto clonedSelf = static_cast<ActorClosureCell*>(cloneCell(actorWorld->cellHeap, closureCell, captureState));

	// Call the actor procedure in the new world but on our thread
	ActorBehaviourCell *behaviourCell = nullptr;

	actorWorld->run([&] (World &world) {
		behaviourCell = clonedSelf->apply(world);
	});

	// Start the receive loop in a new thread
	sched::Dispatcher::defaultInstance().dispatch([=] ()
	{
		receiveLoop(actorWorld, behaviourCell);
	});

	return actorMailbox;
}

}
}
