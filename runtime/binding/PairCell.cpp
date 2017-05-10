#include "PairCell.h"

#include "alloc/allocator.h"

namespace lliby
{

PairCell* PairCell::createInstance(World &world, AnyCell *car, AnyCell *cdr)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) PairCell(car, cdr);
}

}
