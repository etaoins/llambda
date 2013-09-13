#include "StringLikeValue.h"

#include <string.h>

namespace lliby
{
	
bool StringLikeValue::equals(const StringLikeValue &other) const
{
	if (byteLength() != other.byteLength())
	{
		return false;
	}

	return memcmp(utf8Data(), other.utf8Data(), byteLength()) == 0;
}
	
void StringLikeValue::finalize()
{
	delete[] m_utf8Data;
}

}
