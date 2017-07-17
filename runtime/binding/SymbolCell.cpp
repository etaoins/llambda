#include "SymbolCell.h"

#include <string.h>
#include <limits>

#include "binding/StringCell.h"
#include "alloc/allocator.h"
#include "alloc/Heap.h"

#include "unicode/utf8.h"

namespace lliby
{

HeapSymbolCell::HeapSymbolCell(SharedByteArray *byteArray, ByteLengthType byteLength, std::uint32_t charLength) :
	SymbolCell(HeapInlineByteLength),
	m_heapByteLength(byteLength),
	m_heapCharLength(charLength),
	m_heapByteArray(byteArray)
{
}

InlineSymbolCell::InlineSymbolCell(std::uint8_t byteLength, std::uint8_t charLength) :
	SymbolCell(byteLength),
	m_inlineCharLength(charLength)
{
}

std::size_t SymbolCell::inlineDataSize()
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
		auto inlineSymbol = new (cellPlacement) InlineSymbolCell(byteLength, charLength);
		memcpy(inlineSymbol->inlineData(), data, byteLength);

		return inlineSymbol;
	}
	else
	{
		SharedByteArray *newByteArray = SharedByteArray::createUninitialised(byteLength);
		memcpy(newByteArray->data(), data, byteLength);

		return new (cellPlacement) HeapSymbolCell(newByteArray, byteLength, charLength);
	}
}

SymbolCell* SymbolCell::fromString(World &world, StringCell *string)
{
	void *cellPlacement = alloc::allocateCells(world);

	// Symbols must have the same inlining threshold as strings for the below logic to work
	static_assert(
			sizeof(InlineSymbolCell::m_inlineData) == sizeof(InlineStringCell::m_inlineData),
			"Symbols and strings must have the same inlining threshold"
	);

	if (string->dataIsInline())
	{
		auto inlineString = static_cast<InlineStringCell*>(string);

		auto inlineSymbol = new (cellPlacement) InlineSymbolCell(
				inlineString->inlineByteLength(),
				inlineString->inlineCharLength()
		);

		// Copy the inline data over
		const void *srcData = inlineString->inlineData();
		memcpy(inlineSymbol->inlineData(), srcData, inlineString->inlineByteLength());

		return inlineSymbol;
	}
	else
	{
		auto heapString = static_cast<HeapStringCell*>(string);

		// Share the heap strings's byte array
		return new (cellPlacement) HeapSymbolCell(
				heapString->heapByteArray()->ref(),
				heapString->heapByteLength(),
				heapString->heapCharLength()
		);
	}
}

bool SymbolCell::operator==(const SymbolCell &other) const
{
	if (isGlobalConstant() && other.isGlobalConstant())
	{
		// Constant folding guarantees this works
		return this == &other;
	}

	if (byteLength() != other.byteLength())
	{
		return false;
	}

	if (dataIsInline())
	{
		auto thisInlineSymbol = static_cast<const InlineSymbolCell*>(this);
		auto otherInlineSymbol = static_cast<const InlineSymbolCell*>(&other);

		return memcmp(
				thisInlineSymbol->inlineData(),
				otherInlineSymbol->inlineData(),
				thisInlineSymbol->inlineByteLength()) == 0;
	}
	else
	{
		auto thisHeapSymbol = static_cast<const HeapSymbolCell*>(this);

		auto thisByteArray = thisHeapSymbol->heapByteArray();
		auto otherByteArray = static_cast<const HeapSymbolCell*>(&other)->heapByteArray();

		return thisByteArray->isEqual(otherByteArray, thisHeapSymbol->heapByteLength());
	}
}

SymbolCell* SymbolCell::copy(alloc::Heap &heap)
{
	void *cellPlacement = heap.allocate();

	if (dataIsInline())
	{
		auto inlineThis = static_cast<InlineSymbolCell*>(this);

		auto inlineCopy = new (cellPlacement) InlineSymbolCell(
				inlineThis->inlineByteLength(),
				inlineThis->inlineCharLength()
		);

		memcpy(inlineCopy->m_inlineData, constUtf8Data(), inlineThis->inlineByteLength());

		return inlineCopy;
	}
	else
	{
		auto heapThis = static_cast<HeapSymbolCell*>(this);

		return new (cellPlacement) HeapSymbolCell(
				heapThis->heapByteArray()->ref(),
				heapThis->heapByteLength(),
				heapThis->heapCharLength()
		);
	}
}

SharedByteHash::ResultType SymbolCell::sharedByteHash() const
{
	if (dataIsInline())
	{
		auto inlineSymbol = static_cast<const InlineSymbolCell*>(this);

		SharedByteHash byteHasher;
		return byteHasher(inlineSymbol->inlineData(), inlineSymbol->inlineByteLength());
	}
	else
	{
		auto heapSymbol = static_cast<const HeapSymbolCell*>(this);
		return heapSymbol->heapByteArray()->hashValue(heapSymbol->heapByteLength());
	}
}

void SymbolCell::finalizeSymbol()
{
	if (!dataIsInline())
	{
		static_cast<HeapSymbolCell*>(this)->heapByteArray()->unref();
	}
}

}
