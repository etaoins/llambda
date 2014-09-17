#include "ProcedureCell.h"

#include "alloc/allocator.h"

namespace lliby
{
	
ProcedureCell* ProcedureCell::createInstance(World &world, std::uint32_t recordClassId, bool dataIsInline, void *recordData, void *entryPoint)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) ProcedureCell(recordClassId, dataIsInline, recordData, entryPoint);
}

}
