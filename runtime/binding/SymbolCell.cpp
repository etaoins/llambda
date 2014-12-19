#include "SymbolCell.h"

#include <string.h>
#include <limits>

#include "binding/StringCell.h"
#include "alloc/cellref.h"
#include "alloc/allocator.h"

#include "unicode/utf8.h"

namespace lliby
{

HeapSymbolCell::HeapSymbolCell(SharedByteArray *byteArray, ByteLengthType byteLength, std::uint16_t charLength) :
	SymbolCell(byteLength),
	m_charLength(charLength),
	m_heapByteArray(byteArray)
{
}

InlineSymbolCell::InlineSymbolCell(ByteLengthType byteLength) :
	SymbolCell(byteLength)
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

SymbolCell* SymbolCell::fromUtf8Data(World &world, const std::uint8_t *data, ByteLengthType byteLength)
{
	const std::uint8_t *scanPtr = data;
	const std::uint8_t *endPtr = data + byteLength;

	// Validate the UTF-8 data
	const std::size_t charLength = utf8::validateData(scanPtr, endPtr);

	void *cellPlacement = alloc::allocateCells(world);

	if (byteLength <= inlineDataSize())
	{
		auto inlineSymbol = new (cellPlacement) InlineSymbolCell(byteLength);
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

	// Symbols must have a higher inlining threshold than strings for the below logic to work
	static_assert(
			sizeof(InlineSymbolCell::m_inlineData) >= sizeof(InlineStringCell::m_inlineData),
			"Symbols must have a higher inlining threshold than strings"
	);

	auto const byteLength = stringRef->byteLength();

	if (byteLength > maximumByteLength())
	{
		return nullptr;
	}
	else if (byteLength <= inlineDataSize())
	{
		// Create an inline symbol
		auto newInlineSymbol = new (cellPlacement) InlineSymbolCell(
				stringRef->byteLength()
		);

		// Copy the inline data over
		const void *srcData = stringRef->constUtf8Data();
		memcpy(newInlineSymbol->inlineData(), srcData, byteLength);

		return newInlineSymbol;
	}
	else
	{
		auto heapString = static_cast<HeapStringCell*>(stringRef.data());

		// Share the heap string's byte array
		return new (cellPlacement) HeapSymbolCell(
				heapString->heapByteArray()->ref(),
				byteLength,
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
