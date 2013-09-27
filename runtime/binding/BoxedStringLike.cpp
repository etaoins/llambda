#include "BoxedStringLike.h"

#include <string.h>

namespace lliby
{
	
bool BoxedStringLike::equals(const BoxedStringLike &other) const
{
	if (byteLength() != other.byteLength())
	{
		return false;
	}

	return memcmp(utf8Data(), other.utf8Data(), byteLength()) == 0;
}
	
void BoxedStringLike::finalize()
{
	delete[] m_utf8Data;
}

}
