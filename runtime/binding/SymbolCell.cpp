#include "SymbolCell.h"

#include <string.h>

#include "binding/StringCell.h"
#include "alloc/cellref.h"
#include "alloc/allocator.h"

namespace lliby
{

HeapSymbolCell::HeapSymbolCell(SharedByteArray *byteArray, std::uint32_t byteLength, std::uint32_t charLength) :
	SymbolCell(byteLength, charLength),
	m_heapByteArray(byteArray)
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
	
SymbolCell* SymbolCell::fromString(World &world, StringCell *string)
{
	alloc::StringRef stringRef(world, string);
	void *cellPlacement = alloc::allocateCells(world);

	// Strings and symbols must have an identical inline/heap threshold for the below to work
	static_assert(
			sizeof(InlineSymbolCell::m_inlineData) == sizeof(InlineStringCell::m_inlineData), 
			"Symbols and strings must have the same inlining threshold"
	);

	if (stringRef->dataIsInline())
	{
		auto inlineString = static_cast<InlineStringCell*>(stringRef.data());

		// Create an inline symbol
		auto newInlineSymbol = new (cellPlacement) InlineSymbolCell(
				inlineString->byteLength(),
				inlineString->charLength()
		);

		// Copy the inline data over
		const void *srcData = inlineString->inlineData();
		const size_t srcSize = inlineString->byteLength(); 
		memcpy(newInlineSymbol->inlineData(), srcData, srcSize); 

		return newInlineSymbol;
	}
	else
	{
		auto heapString = static_cast<HeapStringCell*>(stringRef.data());

		// Share the heap string's byte array
		return new (cellPlacement) HeapSymbolCell(
				heapString->heapByteArray()->ref(),
				heapString->byteLength(),
				heapString->charLength()
		);
	}
}

bool SymbolCell::dataIsInline() const
{
	return byteLength() <= inlineDataSize();
}
	
const std::uint8_t* SymbolCell::constUtf8Data() const
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

bool SymbolCell::operator==(const SymbolCell &other) const
{
	if (byteLength() != other.byteLength())
	{
		return false;
	}
	
	return memcmp(constUtf8Data(), other.constUtf8Data(), byteLength()) == 0;
}

void SymbolCell::finalizeSymbol() 
{
	if (!dataIsInline())
	{
		static_cast<HeapSymbolCell*>(this)->heapByteArray()->unref();
	}
}

}
