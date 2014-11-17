#include "core/init.h"
#include "dynamic/init.h"

#include "alloc/allocator.h"
#include "alloc/DynamicMemoryBlock.h"

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

void lliby_init(int argc, char *argv[])
{
	// Stash argc and argv
	initArguments = {argc, argv};

	alloc::DynamicMemoryBlock::init();

	dynamic::init();

	// Start the allocator
	alloc::initGlobal();
}

}
