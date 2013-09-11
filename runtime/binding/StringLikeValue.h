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

	// These are NULL safe which is required by R7RS
	bool operator==(const StringLikeValue &other);
	bool operator!=(const StringLikeValue &other)
	{
		return !(*this == other);
	}

	std::uint32_t charLength() const;

	std::int32_t charAt(std::uint32_t offset);
	static const std::int32_t InvalidChar = -1;

protected:
	StringLikeValue(BoxedTypeId typeId, std::uint8_t *utf8Data, std::uint32_t byteLength, bool asciiOnlyHint = false) :
		BoxedDatum(typeId),
		m_asciiOnlyHint(asciiOnlyHint),
		m_byteLength(byteLength),
		m_utf8Data(utf8Data)
	{
	}
	
	static StringLikeValue* fromUtf8CString(BoxedTypeId typeId, const char *str);

private:
	std::uint8_t *charPointer(std::uint32_t offset);
};

}

#endif

