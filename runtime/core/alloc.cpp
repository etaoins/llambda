#include "alloc/allocator.h"
#include "core/World.h"

#include <cstdint>

extern "C"
{

using lliby::alloc::AllocCell;

void *llcore_alloc_cells(lliby::World &world, std::uint64_t count)
{
	return lliby::alloc::allocateCells(world, count);
}

}
