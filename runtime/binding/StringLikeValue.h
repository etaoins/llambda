#ifndef _LLIBY_BINDING_STRINGLIKEVALUE_H
#define _LLIBY_BINDING_STRINGLIKEVALUE_H

#include "BoxedDatum.h"

namespace lliby
{

class StringLikeValue : public BoxedDatum
{
#include "generated/StringLikeValueMembers.h"
public:
	void finalize();

protected:
	// These are NULL safe which is required by R7RS
	bool equals(const StringLikeValue &other) const;

	StringLikeValue(BoxedTypeId typeId, std::uint8_t *utf8Data, std::uint32_t byteLength, bool asciiOnlyHint = false) :
		BoxedDatum(typeId),
		m_asciiOnlyHint(asciiOnlyHint),
		m_byteLength(byteLength),
		m_utf8Data(utf8Data)
	{
	}
};

}

#endif

