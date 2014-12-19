#include "RecordCell.h"

#include "alloc/allocator.h"

namespace lliby
{

RecordCell* RecordCell::createInstance(World &world, RecordClassIdType recordClassId, bool dataIsInline, void *recordData)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) RecordCell(recordClassId, dataIsInline, recordData);
}


}
