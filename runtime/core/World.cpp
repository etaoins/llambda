#include "core/world.h"

#include "core/error.h"

#include "dynamic/State.h"
#include "dynamic/SchemeException.h"
#include "dynamic/State.h"

using namespace lliby;

extern "C"
{

void _lliby_launch_world(void (*entryPoint)(World *))
{
	World *world = new World;

	try
	{
		entryPoint(world);
	}
	catch (dynamic::SchemeException &except)
	{
		// Call all unwind handlers
		dynamic::State::popAllStates();
		fatalError("Unhandled exception", except.object());
	}
	
	dynamic::State::popAllStates();
	alloc::shutdown();

	delete world;
}

}
