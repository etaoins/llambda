#ifndef _LLIBY_BINDING_SYMBOLCELL_H
#define _LLIBY_BINDING_SYMBOLCELL_H

#include "AnyCell.h"

#include <string>

namespace lliby
{

class World;
class StringCell;
class ImplicitSharingTest;

class SymbolCell : public AnyCell
{
#include "generated/SymbolCellMembers.h"
	friend class StringCell;
	friend class ImplicitSharingTest;
public:
	static SymbolCell* fromUtf8StdString(World &world, const std::string &str);
	static SymbolCell* fromUtf8Data(World &world, const std::uint8_t *data, std::uint32_t byteLength);

	static SymbolCell* fromString(World &world, StringCell *string);

	const std::uint8_t* constUtf8Data() const;

	bool operator==(const SymbolCell &other) const;

	bool operator!=(const SymbolCell &other) const
	{
		return !(*this == other);
	}
	
	void finalizeSymbol();

protected:
	SymbolCell(std::uint32_t byteLength, std::uint32_t charLength) :
		AnyCell(CellTypeId::Symbol),
		m_charLength(charLength),
		m_byteLength(byteLength)
	{
	}
	
	static size_t inlineDataSize();
	bool dataIsInline() const;
};

class HeapSymbolCell : public SymbolCell
{
	friend class SymbolCell;
	friend class StringCell;
#include "generated/HeapSymbolCellMembers.h"
private:
	HeapSymbolCell(SharedByteArray *byteArray, std::uint32_t byteLength, std::uint32_t charLength);
};

class InlineSymbolCell : public SymbolCell
{
	friend class SymbolCell;
	friend class StringCell;
#include "generated/InlineSymbolCellMembers.h"
private:
	InlineSymbolCell(std::uint32_t byteLength, std::uint32_t charLength);
};

}

#endif

