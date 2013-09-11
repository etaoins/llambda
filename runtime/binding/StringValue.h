#ifndef _LLIBY_BINDING_STRINGVALUE_H
#define _LLIBY_BINDING_STRINGVALUE_H

#include "StringLikeValue.h"

namespace lliby
{

class StringValue : public StringLikeValue
{
#include "generated/StringValueMembers.h"
public:
	StringValue(std::uint8_t *utf8Data, std::uint32_t byteLength, bool asciiOnlyHint = false) :
		StringLikeValue(BoxedTypeId::String, utf8Data, byteLength, asciiOnlyHint)
	{
	}

	static StringValue* fromUtf8CString(const char *str);

	std::uint32_t charLength() const;

	std::int32_t charAt(std::uint32_t offset);
	static const std::int32_t InvalidChar = -1;

	bool operator==(const StringValue &other) const
	{
		return equals(other);
	}
	
	bool operator!=(const StringValue &other) const
	{
		return !equals(other);
	}

private:
	std::uint8_t *charPointer(std::uint32_t offset);
};

}

#endif

