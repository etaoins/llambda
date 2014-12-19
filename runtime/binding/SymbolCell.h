#ifndef _LLIBY_BINDING_SYMBOLCELL_H
#define _LLIBY_BINDING_SYMBOLCELL_H

#include "AnyCell.h"
#include "SharedByteArray.h"

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
	using ByteLengthType = decltype(m_byteLength);

	constexpr static ByteLengthType maximumByteLength()
	{
		return std::numeric_limits<ByteLengthType>::max();
	}

	/**
	 * Creates a new symbol from an STL string
	 *
	 * This will return nullptr if the string is larger than 64KiB
	 */
	static SymbolCell* fromUtf8StdString(World &world, const std::string &str);

	/**
	 * Creates a new symbol from raw UTF-8 data
	 *
	 * This will return nullptr if the string is larger than 64KiB
	 */
	static SymbolCell* fromUtf8Data(World &world, const std::uint8_t *data, ByteLengthType byteLength);

	/**
	 * Creates a new symbol from a string
	 *
	 * This will return nullptr if the string is larger than 64KiB
	 */
	static SymbolCell* fromString(World &world, StringCell *string);

	const std::uint8_t* constUtf8Data() const;

	bool operator==(const SymbolCell &other) const;

	bool operator!=(const SymbolCell &other) const
	{
		return !(*this == other);
	}
	
	void finalizeSymbol();

protected:
	SymbolCell(ByteLengthType byteLength) :
		AnyCell(CellTypeId::Symbol),
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
	HeapSymbolCell(SharedByteArray *byteArray, ByteLengthType byteLength, std::uint16_t charLength);
};

class InlineSymbolCell : public SymbolCell
{
	friend class SymbolCell;
	friend class StringCell;
#include "generated/InlineSymbolCellMembers.h"
private:
	InlineSymbolCell(ByteLengthType byteLength);
};

}

#endif

