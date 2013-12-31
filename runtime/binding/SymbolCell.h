#ifndef _LLIBY_BINDING_SYMBOLCELL_H
#define _LLIBY_BINDING_SYMBOLCELL_H

#include "DatumCell.h"

namespace lliby
{

class SymbolCell : public DatumCell
{
#include "generated/SymbolCellMembers.h"
public:
	SymbolCell(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength) :
		DatumCell(CellTypeId::Symbol),
		m_charLength(charLength),
		m_byteLength(byteLength),
		m_utf8Data(utf8Data)
	{
	}
	
	bool operator==(const SymbolCell &other) const;

	bool operator!=(const SymbolCell &other) const
	{
		return !(*this == other);
	}
};

}

#endif

