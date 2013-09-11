#ifndef _LLIBY_BINDING_STRINGVALUE_H
#define _LLIBY_BINDING_STRINGVALUE_H

#include "StringLikeValue.h"
#include <list>

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

	typedef std::int32_t CodePoint;
	static const CodePoint InvalidChar = -1;

	static StringValue* fromUtf8CString(const char *str);
	static StringValue* fromFill(std::uint32_t length, CodePoint fill);

	static StringValue* fromAppended(const std::list<const StringValue*> &strings);

	std::uint32_t charLength() const;

	CodePoint charAt(std::uint32_t offset);

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

