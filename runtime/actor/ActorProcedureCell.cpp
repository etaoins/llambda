#include "actor/ActorProcedureCell.h"

#include <thread>

#include "alloc/allocator.h"
#include "actor/cloneCell.h"
#include "dynamic/SchemeException.h"
#include "dynamic/State.h"
#include "core/World.h"

namespace lliby
{
namespace actor
{

void ActorProcedureCell::start()
{
	// Create a new world to launch
	auto *actorWorld = new World;

	alloc::initWorld(*actorWorld);

	// Clone ourselves and our closure in to the new world
	auto clonedSelf = static_cast<ActorProcedureCell*>(cloneCell(actorWorld->cellHeap, this));

	new std::thread([=] () {
		char continuationBase;

		// Use the actor's stack for continuations
		actorWorld->continuationBase = &continuationBase;

		try
		{
			// Call the actor procedure in the new world
			clonedSelf->apply(*actorWorld);
		}
		catch (dynamic::SchemeException &except)
		{
			// XXX: Support supervision
		}

		dynamic::State::popAllStates(*actorWorld);
		alloc::shutdownWorld(*actorWorld);
	});

}

}
}
