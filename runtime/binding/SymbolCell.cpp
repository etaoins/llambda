#include "SymbolCell.h"

#include <string.h>

namespace lliby
{

bool SymbolCell::operator==(const SymbolCell &other) const
{
	if (byteLength() != other.byteLength())
	{
		return false;
	}
	
	return memcmp(utf8Data(), other.utf8Data(), byteLength()) == 0;
}

}
