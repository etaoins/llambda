#ifndef _LLIBY_BINDING_SYMBOLCELL_H
#define _LLIBY_BINDING_SYMBOLCELL_H

#include "DatumCell.h"

namespace lliby
{

class SymbolCell : public DatumCell
{
#include "generated/SymbolCellMembers.h"
public:
	static SymbolCell* fromRawData(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength);

	const std::uint8_t* utf8Data() const;

	bool operator==(const SymbolCell &other) const;

	bool operator!=(const SymbolCell &other) const
	{
		return !(*this == other);
	}

protected:
	SymbolCell(std::uint32_t byteLength, std::uint32_t charLength) :
		DatumCell(CellTypeId::Symbol),
		m_charLength(charLength),
		m_byteLength(byteLength)
	{
	}

	static size_t inlineDataSize();
};

class HeapSymbolCell : public SymbolCell
{
	friend class SymbolCell;
#include "generated/HeapSymbolCellMembers.h"
private:
	HeapSymbolCell(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength);
};

class InlineSymbolCell : public SymbolCell
{
	friend class SymbolCell;
#include "generated/InlineSymbolCellMembers.h"
private:
	InlineSymbolCell(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength);
};

}

#endif

