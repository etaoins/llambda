#include "ProcedureCell.h"

#include "alloc/allocator.h"

namespace lliby
{
	
ProcedureCell* ProcedureCell::createInstance(World &world, std::uint32_t recordClassId, bool dataIsInline, void *recordData, ProcedureEntryPoint entryPoint)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) ProcedureCell(recordClassId, dataIsInline, recordData, entryPoint);
}

ListElementCell* ProcedureCell::apply(World &world, ListElementCell *arguments)
{ 
	return m_entryPoint(world, this, arguments);
}

}
