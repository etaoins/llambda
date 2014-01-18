#include "SymbolCell.h"

#include <string.h>

#include "binding/StringCell.h"
#include "alloc/StrongRef.h"

namespace lliby
{

HeapSymbolCell::HeapSymbolCell(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength) :
	SymbolCell(byteLength, charLength),
	m_heapData(utf8Data)
{
}

InlineSymbolCell::InlineSymbolCell(std::uint32_t byteLength, std::uint32_t charLength) :
	SymbolCell(byteLength, charLength)
{
}

size_t SymbolCell::inlineDataSize()
{
	return sizeof(InlineSymbolCell::m_inlineData);
}

SymbolCell* SymbolCell::createUninitialized(std::uint32_t byteLength, std::uint32_t charLength)
{
	// We need 1 extra byte for the NULL terminator
	const std::int32_t minimumSize = byteLength + 1;

	if (minimumSize <= inlineDataSize())
	{
		// We can fit this symbol inline
		return new InlineSymbolCell(byteLength, charLength);
	}
	else
	{
		// Allocate new space for our heap data
		auto utf8Data = new std::uint8_t[byteLength + 1];
		return new HeapSymbolCell(utf8Data, byteLength, charLength);
	}
}
	
SymbolCell* SymbolCell::fromString(StringCell *string)
{
	alloc::StrongRef<StringCell> stringRef(string);

	// Allocate a new destination symbol
	SymbolCell *newSymbol = SymbolCell::createUninitialized(stringRef->byteLength(), stringRef->charLength());

	// Copy the data over
	memcpy(const_cast<std::uint8_t*>(newSymbol->utf8Data()), stringRef->utf8Data(), stringRef->byteLength() + 1);

	return newSymbol;
}
	
const std::uint8_t* SymbolCell::utf8Data() const
{
	if (byteLength() <= (inlineDataSize() - 1)) 
	{
		return static_cast<const InlineSymbolCell*>(this)->inlineData();
	}
	else
	{
		return static_cast<const HeapSymbolCell*>(this)->heapData();
	}
}

bool SymbolCell::operator==(const SymbolCell &other) const
{
	if (byteLength() != other.byteLength())
	{
		return false;
	}
	
	return memcmp(utf8Data(), other.utf8Data(), byteLength()) == 0;
}

}
