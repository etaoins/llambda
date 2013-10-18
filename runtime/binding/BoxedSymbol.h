#ifndef _LLIBY_BINDING_BOXEDSYMBOL_H
#define _LLIBY_BINDING_BOXEDSYMBOL_H

#include "BoxedStringLike.h"

namespace lliby
{

class BoxedSymbol : public BoxedStringLike
{
#include "generated/BoxedSymbolMembers.h"
public:
	BoxedSymbol(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength) :
		BoxedStringLike(BoxedTypeId::Symbol, utf8Data, byteLength, charLength)
	{
	}
};

}

#endif

