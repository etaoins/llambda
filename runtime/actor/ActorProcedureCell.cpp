#include "actor/ActorProcedureCell.h"

#include "alloc/allocator.h"
#include "alloc/Finalizer.h"

#include "sched/Dispatcher.h"

#include "actor/cloneCell.h"
#include "dynamic/SchemeException.h"
#include "dynamic/State.h"
#include "core/World.h"

namespace lliby
{
namespace actor
{

std::shared_ptr<Mailbox> ActorProcedureCell::start(World &parentWorld)
{
	// Create a new world to launch
	auto *actorWorld = new World;

	// Make sure we grab a reference to the actor's mailbox before we start the thread so we don't race
	std::shared_ptr<Mailbox> actorMailbox(actorWorld->mailbox());

	// Clone ourselves and our closure in to the new world
	dynamic::State *captureState = parentWorld.activeStateCell()->state();
	auto clonedSelf = static_cast<ActorProcedureCell*>(cloneCell(actorWorld->cellHeap, this, captureState));

	sched::Dispatcher::defaultInstance().dispatch([=] () {
		try
		{
			actorWorld->run([=] (World &world) {
				// Call the actor procedure in the new world
				clonedSelf->apply(world);
			});
		}
		catch (dynamic::SchemeException &except)
		{
			// XXX: Support supervision
		}

		delete actorWorld;
	});

	return actorMailbox;
}

}
}
