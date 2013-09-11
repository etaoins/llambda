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

	// Don't depend on asciiOnlyHint() here as it's only a hint
	// For example, our ASCII strings can be mutated to Unicode and back 
	// without regaining their ASCII-only hint
	
	return memcmp(utf8Data(), other.utf8Data(), byteLength()) == 0;
}
	
void StringLikeValue::finalize()
{
	delete m_utf8Data;
}

}
