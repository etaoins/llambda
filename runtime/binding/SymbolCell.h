#ifndef _LLIBY_BINDING_SYMBOLCELL_H
#define _LLIBY_BINDING_SYMBOLCELL_H

#include "AnyCell.h"
#include "SharedByteArray.h"

#include <string>

namespace lliby
{
namespace alloc
{
class Heap;
}

class World;
class StringCell;
class ImplicitSharingTest;

class SymbolCell : public AnyCell
{
#include "generated/SymbolCellMembers.h"
	friend class StringCell;
	friend class ImplicitSharingTest;
public:
	using ByteLengthType = std::uint32_t;

	constexpr static ByteLengthType maximumByteLength()
	{
		return std::numeric_limits<ByteLengthType>::max();
	}

	/**
	 * Creates a new symbol from an STL string
	 */
	static SymbolCell* fromUtf8StdString(World &world, const std::string &str);

	/**
	 * Creates a new symbol from raw UTF-8 data
	 */
	static SymbolCell* fromUtf8Data(World &world, const std::uint8_t *data, ByteLengthType byteLength);

	/**
	 * Creates a new symbol from a string
	 */
	static SymbolCell* fromString(World &world, StringCell *string);

	const std::uint8_t* constUtf8Data() const;

	bool operator==(const SymbolCell &other) const;

	bool operator!=(const SymbolCell &other) const
	{
		return !(*this == other);
	}

	/**
	 * Copies this symbol on to another heap
	 *
	 * This does not make sense within the same world as symbols are immutable
	 */
	SymbolCell* copy(alloc::Heap &heap);

	/**
	 * Returns the length of the symbol's UTF-8 data in bytes
	 */
	ByteLengthType byteLength() const;

	/**
	 * Returns the length of the symbol in Unicode code points
	 */
	std::uint32_t charLength() const;

	/**
	 * Returns the shared byte hash for the UTF-8 data of the symbol
	 *
	 * This is not the same as the datum hash. Use DatumHash to hash the SymbolCell itself.
	 */
	SharedByteHash::ResultType sharedByteHash() const;

	void finalizeSymbol();

protected:
	/**
	 * Value that m_inlineByteLength takes when the symbol is stored on the heap
	 */
	static const std::uint8_t HeapInlineByteLength = 255;

	SymbolCell(std::uint8_t inlineByteLength) :
		AnyCell(CellTypeId::Symbol),
		m_inlineByteLength(inlineByteLength)
	{
	}

	static std::size_t inlineDataSize();
	bool dataIsInline() const;
};

class HeapSymbolCell : public SymbolCell
{
	friend class SymbolCell;
	friend class StringCell;
#include "generated/HeapSymbolCellMembers.h"
private:
	HeapSymbolCell(SharedByteArray *byteArray, ByteLengthType byteLength, std::uint32_t charLength);
};

class InlineSymbolCell : public SymbolCell
{
	friend class SymbolCell;
	friend class StringCell;
#include "generated/InlineSymbolCellMembers.h"
private:
	InlineSymbolCell(std::uint8_t byteLength, std::uint8_t charLength);
};

inline bool SymbolCell::dataIsInline() const
{
	return inlineByteLength() != HeapInlineByteLength;
}

inline const std::uint8_t* SymbolCell::constUtf8Data() const
{
	if (dataIsInline())
	{
		return static_cast<const InlineSymbolCell*>(this)->inlineData();
	}
	else
	{
		return static_cast<const HeapSymbolCell*>(this)->heapByteArray()->data();
	}
}

inline SymbolCell::ByteLengthType SymbolCell::byteLength() const
{
	if (dataIsInline())
	{
		return m_inlineByteLength;
	}
	else
	{
		return static_cast<const HeapSymbolCell*>(this)->heapByteLength();
	}
}

inline std::uint32_t SymbolCell::charLength() const
{
	if (dataIsInline())
	{
		return static_cast<const InlineSymbolCell*>(this)->inlineCharLength();
	}
	else
	{
		return static_cast<const HeapSymbolCell*>(this)->heapCharLength();
	}
}

}

#endif

