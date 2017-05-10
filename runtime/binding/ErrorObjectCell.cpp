#include "binding/ErrorObjectCell.h"
#include "binding/StringCell.h"

#include "alloc/allocator.h"

namespace lliby
{

ErrorObjectCell* ErrorObjectCell::createInstance(World &world, StringCell *message, ProperList<AnyCell> *irritants, ErrorCategory category)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) ErrorObjectCell(message, irritants, category);
}


}
