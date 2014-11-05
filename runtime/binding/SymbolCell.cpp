#include "SymbolCell.h"

#include <string.h>

#include "binding/StringCell.h"
#include "alloc/cellref.h"
#include "alloc/allocator.h"

#include "unicode/utf8.h"

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

SymbolCell* SymbolCell::fromUtf8StdString(World &world, const std::string &str)
{
	return SymbolCell::fromUtf8Data(world, reinterpret_cast<const std::uint8_t *>(str.data()), str.size());
}

SymbolCell* SymbolCell::fromUtf8Data(World &world, const std::uint8_t *data, std::uint32_t byteLength)
{
	const std::uint8_t *scanPtr = data;
	const std::uint8_t *endPtr = data + byteLength;

	// Find the character length - this can throw an exception
	size_t charLength = utf8::validateData(scanPtr, endPtr);

	void *cellPlacement = alloc::allocateCells(world);

	if (byteLength <= inlineDataSize())
	{
		auto inlineSymbol = new (cellPlacement) InlineSymbolCell(byteLength, charLength);
		memcpy(inlineSymbol->inlineData(), data, byteLength);

		return inlineSymbol;
	}
	else
	{
		SharedByteArray *newByteArray = SharedByteArray::createInstance(byteLength);
		memcpy(newByteArray->data(), data, byteLength);

		auto heapSymbol = new (cellPlacement) HeapSymbolCell(
				newByteArray,
				byteLength,
				charLength
		);

		return heapSymbol;
	}
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

	if (constUtf8Data() == other.constUtf8Data())
	{
		// We're either the same cell or implicitly sharing the same data
		return true;
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
