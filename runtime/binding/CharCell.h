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
	explicit CharCell(UnicodeChar unicodeChar) :
		AnyCell(CellTypeId::Char),
		m_unicodeChar(unicodeChar)
	{
	}

	static CharCell* createInstance(World &world, UnicodeChar unicodeChar);

	static CharCell* createInstance(World &world, std::int32_t codePoint)
	{
		return createInstance(world, UnicodeChar(codePoint));
	}

};

}


#endif
