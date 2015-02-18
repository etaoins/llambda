#include "core/init.h"
#include "dynamic/init.h"
#include "core/World.h"

#include "core/error.h"
#include "alloc/allocator.h"
#include "alloc/MemoryBlock.h"

#include "dynamic/SchemeException.h"

namespace lliby
{
namespace
{
	CommandLineArguments initArguments {0, nullptr};
}

CommandLineArguments commandLineArguments()
{
	return initArguments;
}

}

extern "C"
{
using namespace lliby;

void llcore_run(void (*entryPoint)(lliby::World &), int argc, char **argv, bool skipFinal)
{
	// Stash argc and argv
	initArguments = {argc, argv};

	dynamic::init();

	alloc::initGlobal();

	{
		// Make sure the world is alive for the exception handler
		World rootWorld;

		try
		{
			rootWorld.run(entryPoint);
		}
		catch (dynamic::SchemeException &except)
		{
			fatalError("Unhandled exception", except.object());
		}

#ifndef _LLIBY_CHECK_LEAKS
		if (skipFinal)
		{
			// Intentionally leak all of the world's cells
			rootWorld.cellHeap.detach();

		}
#endif
	}

	alloc::shutdownGlobal();
}

}
