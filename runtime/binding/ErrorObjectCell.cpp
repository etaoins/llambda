#include "binding/ErrorObjectCell.h"
#include "binding/StringCell.h"

#include "alloc/StrongRef.h"

namespace lliby
{
	
ErrorObjectCell* ErrorObjectCell::createInstance(StringCell *message, ListElementCell *irritants)
{
	alloc::StrongRefRange<StringCell> messageRoot(&message, 1);
	alloc::StrongRefRange<ListElementCell> irritantsRoot(&irritants, 1);

	return new ErrorObjectCell(message, irritants);
}


}
