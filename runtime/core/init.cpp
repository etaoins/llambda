#include "core/init.h"
#include "dynamic/init.h"

#include "alloc/allocator.h"
#include "alloc/DynamicMemoryBlock.h"

extern "C"
{
using namespace lliby;

void lliby_init()
{
	alloc::DynamicMemoryBlock::init();

	dynamic::init();

	// Start the allocator
	alloc::initGlobal();
}

}
