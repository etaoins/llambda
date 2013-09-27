#ifndef _LLIBY_BINDING_BOXEDCHARACTER_H
#define _LLIBY_BINDING_BOXEDCHARACTER_H

#include "BoxedDatum.h"
#include "unicode/UnicodeChar.h"

namespace lliby
{

class BoxedCharacter : public BoxedDatum
{
#include "generated/BoxedCharacterMembers.h"
public:
	BoxedCharacter(UnicodeChar unicodeChar) :
		BoxedDatum(BoxedTypeId::Character),
		m_unicodeChar(unicodeChar)
	{
	}
};
	
}


#endif
