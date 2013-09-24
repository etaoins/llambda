#ifndef _LLIBY_BINDING_CHARACTERVALUE_H
#define _LLIBY_BINDING_CHARACTERVALUE_H

#include "BoxedDatum.h"
#include "unicode/UnicodeChar.h"

namespace lliby
{

class CharacterValue : public BoxedDatum
{
#include "generated/CharacterValueMembers.h"
public:
	CharacterValue(UnicodeChar unicodeChar) :
		BoxedDatum(BoxedTypeId::Character),
		m_unicodeChar(unicodeChar)
	{
	}
};
	
}


#endif
