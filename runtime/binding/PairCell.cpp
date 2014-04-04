#include "PairCell.h"

#include "alloc/allocator.h"
#include "alloc/cellref.h"

namespace lliby
{

PairCell* PairCell::createInstance(World &world, DatumCell *car, DatumCell *cdr)
{
	// Root the car and cdr for the next allocation
	alloc::DatumRef carRef(world, car);
	alloc::DatumRef cdrRef(world, cdr);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) PairCell(carRef, cdrRef);
}

}
