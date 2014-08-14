#include "PairCell.h"

#include "alloc/allocator.h"
#include "alloc/cellref.h"

namespace lliby
{

PairCell* PairCell::createInstance(World &world, AnyCell *car, AnyCell *cdr)
{
	// Root the car and cdr for the next allocation
	alloc::AnyRef carRef(world, car);
	alloc::AnyRef cdrRef(world, cdr);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) PairCell(carRef, cdrRef);
}

}
