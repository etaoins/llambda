#include "DynamicStateCell.h"

#include "alloc/allocator.h"
#include "dynamic/State.h"

namespace lliby
{

DynamicStateCell* DynamicStateCell::createInstance(World &world, dynamic::State *state)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) DynamicStateCell(state, GarbageState::AllocatedCell);
}

void DynamicStateCell::finalizeDynamicState()
{
	delete state();
}

}
