#ifndef _LLIBY_BINDING_CHARACTERCELL_H
#define _LLIBY_BINDING_CHARACTERCELL_H

#include "DatumCell.h"
#include "unicode/UnicodeChar.h"

namespace lliby
{
class World;

class CharacterCell : public DatumCell
{
#include "generated/CharacterCellMembers.h"
public:
	static CharacterCell* createInstance(World &world, UnicodeChar unicodeChar);

protected:
	CharacterCell(UnicodeChar unicodeChar) :
		DatumCell(CellTypeId::Character),
		m_unicodeChar(unicodeChar)
	{
	}
};
	
}


#endif
