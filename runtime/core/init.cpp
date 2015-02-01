#include "core/init.h"
#include "dynamic/init.h"
#include "core/World.h"

#include "core/error.h"
#include "alloc/allocator.h"
#include "alloc/DynamicMemoryBlock.h"

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

void llcore_run(void (*entryPoint)(lliby::World &), int argc, char **argv)
{
	// Stash argc and argv
	initArguments = {argc, argv};

	alloc::DynamicMemoryBlock::init();

	dynamic::init();

	alloc::initGlobal();

	try
	{
		World rootWorld;
		rootWorld.run(entryPoint);
	}
	catch (dynamic::SchemeException &except)
	{
		fatalError("Unhandled exception", except.object());
	}

	alloc::shutdownGlobal();
}

}
