#ifndef _LLIBY_BINDING_CHARCELL_H
#define _LLIBY_BINDING_CHARCELL_H

#include "AnyCell.h"
#include "unicode/UnicodeChar.h"

namespace lliby
{
class World;

class CharCell : public AnyCell
{
#include "generated/CharCellMembers.h"
public:
	static CharCell* createInstance(World &world, UnicodeChar unicodeChar);

protected:
	CharCell(UnicodeChar unicodeChar) :
		AnyCell(CellTypeId::Char),
		m_unicodeChar(unicodeChar)
	{
	}
};
	
}


#endif
