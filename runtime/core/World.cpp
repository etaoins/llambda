#include "core/world.h"

#include "core/error.h"

#include "alloc/CellRefRangeList.h"

#include "dynamic/State.h"
#include "dynamic/SchemeException.h"
#include "dynamic/State.h"

using namespace lliby;

namespace
{

World *currentActiveWorld = nullptr;

}

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

World& World::activeWorld()
{
	return *currentActiveWorld;
}

void World::launchWorld(void (*entryPoint)(World &))
{
	World world;

	// XXX: Remove me
	currentActiveWorld = &world;

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
	alloc::shutdown();

	// XXX: Remove me
	currentActiveWorld = nullptr;
}

}

extern "C"
{

void _lliby_launch_world(void (*entryPoint)(World &))
{
	World::launchWorld(entryPoint);
}

}
