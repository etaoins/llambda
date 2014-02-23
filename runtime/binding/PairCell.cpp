#include "PairCell.h"

#include "alloc/allocator.h"
#include "alloc/StrongRef.h"

namespace lliby
{

PairCell* PairCell::createInstance(World &world, DatumCell *car, DatumCell *cdr)
{
	// Root the car and cdr for the next allocation
	alloc::StrongRef<DatumCell> carRef(world, car);
	alloc::StrongRef<DatumCell> cdrRef(world, cdr);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) PairCell(carRef, cdrRef);
}

}
