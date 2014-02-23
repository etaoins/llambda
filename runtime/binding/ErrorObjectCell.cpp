#include "binding/ErrorObjectCell.h"
#include "binding/StringCell.h"

#include "alloc/StrongRef.h"
#include "alloc/allocator.h"

namespace lliby
{
	
ErrorObjectCell* ErrorObjectCell::createInstance(World &world, StringCell *message, ListElementCell *irritants)
{
	alloc::StrongRefRange<StringCell> messageRoot(world, &message, 1);
	alloc::StrongRefRange<ListElementCell> irritantsRoot(world, &irritants, 1);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) ErrorObjectCell(message, irritants);
}


}
