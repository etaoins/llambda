#include "StringLikeCell.h"

#include <string.h>

namespace lliby
{
	
bool StringLikeCell::equals(const StringLikeCell &other) const
{
	if (byteLength() != other.byteLength())
	{
		return false;
	}

	return memcmp(utf8Data(), other.utf8Data(), byteLength()) == 0;
}
	
void StringLikeCell::finalize()
{
	delete[] m_utf8Data;
}

}
