#ifndef _LLIBY_BINDING_SYMBOLVALUE_H
#define _LLIBY_BINDING_SYMBOLVALUE_H

#include "StringLikeValue.h"

namespace lliby
{

class SymbolValue : public StringLikeValue
{
#include "generated/StringValueMembers.h"
public:
	SymbolValue(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength) :
		StringLikeValue(BoxedTypeId::Symbol, utf8Data, byteLength, charLength)
	{
	}
};

}

#endif

