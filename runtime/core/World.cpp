#include "core/World.h"

#include "core/error.h"

#include "alloc/CellRefRangeList.h"
#include "alloc/allocator.h"

#include "dynamic/State.h"
#include "dynamic/SchemeException.h"
#include "dynamic/State.h"

using namespace lliby;

namespace lliby
{

World::World() :
	activeState(new dynamic::State(nullptr, nullptr)),
	strongRefs(new alloc::CellRefRangeList),
	weakRefs(new alloc::CellRefRangeList)
{
}

World::~World()
{
	delete activeState;
	delete strongRefs;
	delete weakRefs;
}

void World::launchWorld(void (*entryPoint)(World &))
{
	World world;

	alloc::initWorld(world);

	try
	{
		entryPoint(world);
	}
	catch (dynamic::SchemeException &except)
	{
		// Call all unwind handlers
		dynamic::State::popAllStates(world);
		fatalError("Unhandled exception", except.object());
	}
	
	dynamic::State::popAllStates(world);
	alloc::shutdownWorld(world);
}

}

extern "C"
{

void _lliby_launch_world(void (*entryPoint)(World &))
{
	World::launchWorld(entryPoint);
}

}
