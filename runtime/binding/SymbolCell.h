#ifndef _LLIBY_BINDING_SYMBOLCELL_H
#define _LLIBY_BINDING_SYMBOLCELL_H

#include "StringLikeCell.h"

namespace lliby
{

class SymbolCell : public StringLikeCell
{
#include "generated/SymbolCellMembers.h"
public:
	SymbolCell(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength) :
		StringLikeCell(CellTypeId::Symbol, utf8Data, byteLength, charLength)
	{
	}
};

}

#endif

