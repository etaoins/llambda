#include "SymbolCell.h"

#include <string.h>

namespace lliby
{

HeapSymbolCell::HeapSymbolCell(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength) :
	SymbolCell(byteLength, charLength)
{
	// Allocate new space for our heap data
	m_heapData = new std::uint8_t[byteLength + 1];
	
	// Copy the UTF-8 in
	memcpy(m_heapData, utf8Data, byteLength + 1);
}

InlineSymbolCell::InlineSymbolCell(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength) :
	SymbolCell(byteLength, charLength)
{
	// Copy in to our inline data area
	memcpy(inlineData(), utf8Data, byteLength + 1);
}

size_t SymbolCell::inlineDataSize()
{
	return sizeof(InlineSymbolCell::m_inlineData);
}
	
SymbolCell* SymbolCell::fromRawData(std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength)
{
	// Need room for the NULL terminator
	if (byteLength <= (inlineDataSize() - 1)) 
	{
		return new InlineSymbolCell(utf8Data, byteLength, charLength);
	}
	else
	{
		return new HeapSymbolCell(utf8Data, byteLength, charLength);
	}
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
