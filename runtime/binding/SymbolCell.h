#ifndef _LLIBY_BINDING_SYMBOLCELL_H
#define _LLIBY_BINDING_SYMBOLCELL_H

#include "DatumCell.h"

namespace lliby
{

class World;
class StringCell;

class SymbolCell : public DatumCell
{
#include "generated/SymbolCellMembers.h"
public:
	static SymbolCell* fromString(World &world, StringCell *string);

	const std::uint8_t* utf8Data() const;

	bool operator==(const SymbolCell &other) const;

	bool operator!=(const SymbolCell &other) const
	{
		return !(*this == other);
	}
	
	void finalizeSymbol();

protected:
	SymbolCell(std::uint32_t byteLength, std::uint32_t charLength) :
		DatumCell(CellTypeId::Symbol),
		m_charLength(charLength),
		m_byteLength(byteLength)
	{
	}
	
	static SymbolCell* createUninitialized(std::uint32_t byteLength, std::uint32_t charLength);

	static size_t inlineDataSize();
	bool dataIsInline() const;
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
	InlineSymbolCell(std::uint32_t byteLength, std::uint32_t charLength);
};

}

#endif

