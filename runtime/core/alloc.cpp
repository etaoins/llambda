#include "alloc/allocator.h"
#include "core/World.h"

#include <cstdint>

extern "C"
{

using lliby::alloc::AllocCell;

void *llcore_alloc_cells(lliby::World &world, std::uint64_t count)
{
	void *result = lliby::alloc::allocateCells(world, count);

#ifdef _LLIBY_ALWAYS_GC
	// Force the program to re-enter the garbage collector on the next allocation
	world.cellHeap.sealCurrentSegment();
#endif

	return result;
}

}
