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

	static StringValue* fromUtf8CString(const char *str)
	{
		return reinterpret_cast<StringValue*>(StringLikeValue::fromUtf8CString(BoxedTypeId::String, str));
	}
};

}

#endif

