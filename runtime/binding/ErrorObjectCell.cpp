#include "binding/ErrorObjectCell.h"
#include "binding/StringCell.h"

#include "alloc/StrongRef.h"

namespace lliby
{
	
ErrorObjectCell* ErrorObjectCell::createInstance(World &world, StringCell *message, ListElementCell *irritants)
{
	alloc::StrongRefRange<StringCell> messageRoot(world, &message, 1);
	alloc::StrongRefRange<ListElementCell> irritantsRoot(world, &irritants, 1);

	return new ErrorObjectCell(message, irritants);
}


}
