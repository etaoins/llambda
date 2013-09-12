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
	static StringValue* fromCodePoints(const std::list<CodePoint> &codePoints);

	StringValue* copy(std::int64_t start = 0, std::int64_t end = -1); 

	std::uint32_t charLength() const;

	CodePoint charAt(std::uint32_t offset) const;
	bool setCharAt(std::uint32_t offset, CodePoint codePoint);

	bool fill(CodePoint codePoint, std::int64_t start = 0, std::int64_t end = -1);

	std::list<CodePoint> codePoints(std::int64_t start = 0, std::int64_t end = -1) const;

	bool operator==(const StringValue &other) const
	{
		return equals(other);
	}
	
	bool operator!=(const StringValue &other) const
	{
		return !equals(other);
	}

private:
	std::uint8_t *charPointer(std::uint8_t *scanFrom, std::uint32_t bytesLeft, uint32_t charOffset) const;
	std::uint8_t *charPointer(std::uint32_t charOffset) const;
};

}

#endif

