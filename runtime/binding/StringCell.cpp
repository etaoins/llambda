#include "StringCell.h"

#include <string.h>
#include <algorithm>
#include <limits>
#include <cassert>

#include "SymbolCell.h"
#include "BytevectorCell.h"

#include "platform/memory.h"
#include "unicode/utf8.h"
#include "util/StringCellBuilder.h"

#include "alloc/allocator.h"
#include "alloc/cellref.h"

namespace lliby
{

StringCell* StringCell::createUninitialized(World &world, std::uint32_t byteLength, std::uint32_t charLength)
{
	void *cellPlacement = alloc::allocateCells(world);

	if (byteLength <= inlineDataSize())
	{
		// We can fit this string inline
		auto newString = new (cellPlacement) InlineStringCell(byteLength, charLength);

#ifndef NDEBUG
		if (byteLength < inlineDataSize())
		{
			// Explicitly terminate with non-NULL to catch users that assume we're NULL terminated internally
			newString->inlineData()[byteLength] = 0xff;
		}
#endif

		return newString;
	}
	else
	{
		// Allocate a new shared byte array
		SharedByteArray *newByteArray = SharedByteArray::createInstance(byteLength);

#ifndef NDEBUG
		if (newByteArray->capacity(byteLength) > byteLength)
		{
			newByteArray->data()[byteLength] = 0xff;
		}
#endif

		return new (cellPlacement) HeapStringCell(newByteArray, byteLength, charLength);
	}
}

StringCell* StringCell::fromUtf8StdString(World &world, const std::string &str)
{
	return StringCell::fromUtf8Data(world, reinterpret_cast<const std::uint8_t*>(str.data()), str.size());
}

StringCell* StringCell::withUtf8ByteArray(World &world, SharedByteArray *byteArray, std::uint32_t byteLength)
{
	if (byteLength <= inlineDataSize())
	{
		// We can't use the byte array directly
		return fromUtf8Data(world, byteArray->data(), byteLength);
	}

	const std::uint8_t *scanPtr = byteArray->data();
	const std::uint8_t *endPtr = scanPtr + byteLength;

	// Calculate the character length - this can throw an exception
	size_t charLength = utf8::validateData(scanPtr, endPtr);

	// Reference the byte array now that its contents have been validated
	byteArray->ref();

	// Create a new heap cell sharing the byte array
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) HeapStringCell(byteArray, byteLength, charLength);
}

StringCell* StringCell::fromValidatedUtf8Data(World &world, const std::uint8_t *data, std::uint32_t byteLength, std::uint32_t charLength)
{
	auto newString = StringCell::createUninitialized(world, byteLength, charLength);

	std::uint8_t *utf8Data = newString->utf8Data();
	memcpy(utf8Data, data, byteLength);

	return newString;
}

StringCell* StringCell::fromUtf8Data(World &world, const std::uint8_t *data, std::uint32_t byteLength)
{
	const std::uint8_t *scanPtr = data;
	const std::uint8_t *endPtr = data + byteLength;

	// Find the character length - this can throw an exception
	size_t charLength = utf8::validateData(scanPtr, endPtr);

	return StringCell::fromValidatedUtf8Data(world, data, byteLength, charLength);
}

StringCell* StringCell::fromFill(World &world, std::uint32_t length, UnicodeChar fill)
{
	// Figure out how many bytes we'll need
	utf8::EncodedChar encoded(utf8::encodeChar(fill));
	const std::size_t encodedCharSize = encoded.size;

	const std::uint32_t byteLength = encodedCharSize * length;

	// Allocate the string
	auto newString = StringCell::createUninitialized(world, byteLength, length);

	std::uint8_t *utf8Data = newString->utf8Data();

	// Actually fill
	for(std::uint32_t i = 0; i < length; i++)
	{
		memcpy(&utf8Data[i * encodedCharSize], encoded.data, encodedCharSize);
	}

	return newString;
}
	
StringCell* StringCell::fromAppended(World &world, std::vector<StringCell*> &strings)
{
	if (strings.size() == 1)
	{
		// This allows implicit data sharing while the below always allocates
		return strings.front()->copy(world);
	}
	
	std::uint64_t totalByteLength = 0;
	std::uint32_t totalCharLength = 0;

	for(auto stringPart : strings)
	{
		totalByteLength += stringPart->byteLength();
		totalCharLength += stringPart->charLength();
	}

	// We only have to check bytelength because charLength must always be <=
	if (totalByteLength > std::numeric_limits<std::uint32_t>::max())
	{
		return nullptr;
	}

	// Mark our input strings as GC roots
	alloc::StrongRoot<StringCell> inputRoots(world, strings.data(), strings.size());

	// Allocate the new string
	auto newString = StringCell::createUninitialized(world, totalByteLength, totalCharLength);

	std::uint8_t *copyPtr = newString->utf8Data();

	// Copy all the string parts over
	for(auto stringPart : strings)
	{
		memcpy(copyPtr, stringPart->utf8Data(), stringPart->byteLength());
		copyPtr += stringPart->byteLength();
	}

	return newString;
}

StringCell* StringCell::fromSymbol(World &world, SymbolCell *symbol)
{
	alloc::SymbolRef symbolRef(world, symbol);

	const auto byteLength = symbolRef->byteLength();

	if (symbolRef->dataIsInline())
	{
		// Calculate the symbol's length
		// Symbols are guaranteed to be valid UTF-8 but we trade the space to store the character length for additional
		// inline space
		const std::uint8_t *scanPtr = symbolRef->constUtf8Data();
		const std::uint8_t *endPtr = symbolRef->constUtf8Data() + byteLength;
		const size_t charLength = utf8::countChars(scanPtr, endPtr);

		// Create a string - this may be heap or inline depending on the length of the symbol
		StringCell *newString = createUninitialized(world, byteLength, charLength);

		// Copy the inline data over
		auto inlineSymbol = static_cast<InlineSymbolCell*>(symbolRef.data());

		const void *srcData = inlineSymbol->inlineData();
		const size_t srcSize = inlineSymbol->byteLength();
		memcpy(newString->utf8Data(), srcData, srcSize);

		return newString;
	}
	else
	{
		void *cellPlacement = alloc::allocateCells(world);
		auto heapSymbol = static_cast<HeapSymbolCell*>(symbolRef.data());

		// Share the heap symbols's byte array
		return new (cellPlacement) HeapStringCell(
				heapSymbol->heapByteArray()->ref(),
				heapSymbol->byteLength(),
				heapSymbol->charLength()
		);
	}
}

size_t StringCell::inlineDataSize()
{
	return sizeof(InlineStringCell::m_inlineData);
}
	
bool StringCell::dataIsInline() const
{
	return byteLength() <= inlineDataSize();
}

std::size_t StringCell::byteCapacity() const
{
	if (dataIsInline())
	{
		return inlineDataSize();
	}
	else
	{
		return static_cast<const HeapStringCell*>(this)->heapByteArray()->capacity(byteLength());
	}
}

std::uint8_t* StringCell::utf8Data()
{
	if (dataIsInline())
	{
		return static_cast<InlineStringCell*>(this)->inlineData();
	}
	else
	{
		return static_cast<HeapStringCell*>(this)->heapByteArray()->data();
	}
}

const std::uint8_t* StringCell::constUtf8Data() const
{
	return const_cast<StringCell*>(this)->utf8Data();
}

const std::uint8_t* StringCell::charPointer(std::uint32_t charOffset)
{
	return charPointer(charOffset, utf8Data(), 0);
}

const std::uint8_t* StringCell::charPointer(std::uint32_t charOffset, const std::uint8_t *startFrom, std::uint32_t startOffset)
{
	// Is the rest of the string ASCII?
	// We can determine this by verifying the number remaining bytes in the string are equal to the number of remaining
	// characters.
	const auto bytesLeft = &utf8Data()[byteLength()] - startFrom;
	const auto charsLeft = charLength() - startOffset;

	if (bytesLeft == charsLeft)
	{
		return startFrom + (charOffset - startOffset);
	}

	const std::uint8_t *scanPtr;

	// Should we do a forward scan or backwards scan?
	// Prefer forwards slightly as it probably plays better with hardware memory prefetch
	if ((charOffset - startOffset) > (charsLeft / 2))
	{
		scanPtr = &utf8Data()[byteLength()];

		auto endOffset = charLength();
		while(endOffset > charOffset)
		{
			if (!utf8::isContinuationByte(*(--scanPtr)))
			{
				endOffset--;
			}
		}
	}
	else
	{
		scanPtr = startFrom;
		while(startOffset < charOffset)
		{
			if (!utf8::isContinuationByte(*(++scanPtr)))
			{
				startOffset++;
			}
		}
	}

	return scanPtr;
}

StringCell::CharRange StringCell::charRange(std::int64_t start, std::int64_t end)
{
	if (end == -1)
	{
		end = charLength();
	}
	else if (end > charLength())
	{
		// The end can't be greater than the string length
		return CharRange { 0 };
	}

	if (end < start)
	{
		// The end can't be before the start
		// Combined with the above logic that means the start must also be less than or equal to the length
		return CharRange { 0 };
	}

	const std::uint32_t charCount = end - start;

	const std::uint8_t *startPointer = charPointer(start);
	const std::uint8_t *endPointer = charPointer(end, startPointer, start);

	return CharRange { startPointer, endPointer, charCount };
}

UnicodeChar StringCell::charAt(std::uint32_t offset) const
{
	if (offset >= charLength())
	{
		return UnicodeChar();
	}

	const std::uint8_t *charPtr = const_cast<StringCell*>(this)->charPointer(offset);
	return utf8::decodeChar(&charPtr);
}

bool StringCell::replaceBytes(const CharRange &range, const std::uint8_t *pattern, unsigned int patternBytes, unsigned int count)
{
	assert(!isGlobalConstant());

	const unsigned int requiredBytes = patternBytes * count;
	const unsigned int replacedBytes = range.byteCount();
	
	// If we have exclusive access to our data and we're not resizing the string we can use the fast path
	if ((dataIsInline() || static_cast<HeapStringCell*>(this)->heapByteArray()->isExclusive()) &&
	    (requiredBytes == replacedBytes))
	{
		std::uint8_t *copyDest = const_cast<std::uint8_t*>(range.startPointer);
		while(count--)
		{
			memmove(copyDest, pattern, patternBytes);
			copyDest += patternBytes;
		}
	}
	else
	{
		// Create a new string from pieces of the old string
		const std::uint64_t newByteLength = byteLength() + requiredBytes - replacedBytes;
		
		if (newByteLength > std::numeric_limits<std::uint32_t>::max())
		{
			return false;
		}

		const std::uint32_t initialBytes = range.startPointer - utf8Data(); 
		const std::uint32_t finalBytes = newByteLength - initialBytes - requiredBytes;

		const bool wasInline = dataIsInline();
		const bool nowInline = newByteLength <= inlineDataSize();

		SharedByteArray *oldByteArray = nullptr;
		SharedByteArray *newByteArray = nullptr;

		// Does this require a COW due to sharing our byte array?
		const bool needsCow = (!wasInline && !nowInline) &&
			                   !static_cast<HeapStringCell*>(this)->heapByteArray()->isExclusive();

		// Determine if we exceeded our current capacity or if we're using less than half of our allocated space
		// This will trigger a reallocation of our heap space
		const auto currentCapacity = byteCapacity();
		const bool needHeapRealloc = (newByteLength > currentCapacity) ||
			                         ((newByteLength < (currentCapacity / 2)) && !nowInline) ||
			                         needsCow;

		std::uint8_t* destString;
		const std::uint8_t* copySource;

		if (!wasInline && nowInline)
		{
			// We're converting to an inline string
			destString = static_cast<InlineStringCell*>(this)->inlineData();
			copySource = pattern;

			// Store our old byte array so we can unref it later
			// The code below will overwrite it with our new inline string
			oldByteArray = static_cast<HeapStringCell*>(this)->heapByteArray();

			// Fill the initial chunk of the string
			memcpy(destString, utf8Data(), initialBytes);
		}
		else if (needHeapRealloc)
		{
			size_t byteArraySize = newByteLength;

			newByteArray = SharedByteArray::createInstance(byteArraySize);
			destString = newByteArray->data();
			copySource = pattern;

			// Fill the initial chunk of the string
			memcpy(destString, utf8Data(), initialBytes);

			if (!wasInline)
			{
				// Store our old byte array so we can unref it later
				oldByteArray = static_cast<HeapStringCell*>(this)->heapByteArray();
			}
		}
		else
		{
			destString = utf8Data();

			// The initial chunk is already correct

			// Are our pattern bytes in the range we're about to overwrite?
			// We only need to check the end of the pattern because the pattern should only be completely inside our
			// completely outside our string
			if (((pattern + patternBytes) > (utf8Data() + initialBytes)) &&
				((pattern + patternBytes) <= (utf8Data() + byteLength())))
			{
				// Create a temporary copy to work with
				copySource = new std::uint8_t[patternBytes];
				memcpy(const_cast<std::uint8_t*>(copySource), pattern, patternBytes);
			}
			else
			{
				copySource = pattern;
			}
		}

		// Move the unchanged chunk at the end
		// We need to do this now because if the pattern bytes are longer than the byte we're replacing then we might
		// overwrite the beginning of the unchanged chunk
		memmove(destString + initialBytes + requiredBytes, range.startPointer + replacedBytes, finalBytes);
		
		std::uint8_t* copyDest = destString + initialBytes;

		while(count--)
		{
			memcpy(copyDest, copySource, patternBytes);
			copyDest += patternBytes;
		}

		if (copySource != pattern)
		{
			delete[] copySource;
		}

		// Update ourselves with our new string
		setByteLength(newByteLength);

		if (newByteArray)
		{
			static_cast<HeapStringCell*>(this)->setHeapByteArray(newByteArray);
		}

		if (oldByteArray != nullptr)
		{
			// We can unref this now
			oldByteArray->unref();
		}
	}
	
	return true;
}

bool StringCell::fill(UnicodeChar unicodeChar, std::int64_t start, std::int64_t end)
{
	assert(unicodeChar.isValid());
	CharRange range = charRange(start, end);

	if (range.isNull())
	{
		// Invalid range
		return false;
	}

	// Encode the new character
	utf8::EncodedChar encoded(utf8::encodeChar(unicodeChar));
	return replaceBytes(range, encoded.data, encoded.size, range.charCount);
}

bool StringCell::replace(std::uint32_t offset, const StringCell *from, std::int64_t fromStart, std::int64_t fromEnd)
{
	CharRange fromRange = const_cast<StringCell*>(from)->charRange(fromStart, fromEnd);

	if (fromRange.isNull())
	{
		return false;
	}

	CharRange toRange = charRange(offset, offset + fromRange.charCount);

	if (toRange.isNull() || (toRange.charCount != fromRange.charCount))
	{
		return false;
	}

	return replaceBytes(toRange, fromRange.startPointer, fromRange.byteCount(), 1);
}

bool StringCell::setCharAt(std::uint32_t offset, UnicodeChar unicodeChar)
{
	assert(unicodeChar.isValid());
	return fill(unicodeChar, offset, offset + 1);
}

StringCell* StringCell::copy(World &world, std::int64_t start, std::int64_t end)
{
	// Allocating a string below can actually change "this"
	// That is super annoying
	StringCell *oldThis = const_cast<StringCell*>(this);
	alloc::StringRef thisRef(world, oldThis);

	CharRange range = charRange(start, end);

	if (range.isNull())
	{
		// Invalid range
		return nullptr;
	}

	if ((range.charCount == charLength()) && !dataIsInline())
	{
		// We're copying the whole string
		// Share our byte array
		void *cellPlacement = alloc::allocateCells(world);
		HeapStringCell *heapThis = static_cast<HeapStringCell*>(thisRef.data());

		return new (cellPlacement) HeapStringCell(
				heapThis->heapByteArray()->ref(),
				heapThis->byteLength(),
				heapThis->charLength()
		);
	}

	const std::uint32_t newByteLength = range.byteCount();

	// Create the new string
	auto newString = StringCell::createUninitialized(world, newByteLength, range.charCount);
	
	if (thisRef->dataIsInline() && (oldThis != thisRef.data()))
	{
		// The allocator ran and moved us along with our inline data
		// We have to update our range
		ptrdiff_t byteDelta = reinterpret_cast<std::uint8_t*>(thisRef.data()) -
			                   reinterpret_cast<std::uint8_t*>(oldThis);

		range.relocate(byteDelta);
	}

	std::uint8_t *newUtf8Data = newString->utf8Data();

	memcpy(newUtf8Data, range.startPointer, newByteLength);

	return newString;
}
	
std::vector<UnicodeChar> StringCell::unicodeChars(std::int64_t start, std::int64_t end) const
{
	if (end == -1)
	{
		end = charLength();
	}

	if ((end > charLength()) || (end < start))
	{
		// Invalid range
		return std::vector<UnicodeChar>();
	}

	const std::uint8_t *scanPtr = const_cast<StringCell*>(this)->charPointer(start);

	if (scanPtr == nullptr)
	{
		return std::vector<UnicodeChar>();
	}

	const std::uint32_t charCount = end - start;

	std::vector<UnicodeChar> ret;
	ret.reserve(charCount);

	while(ret.size() < charCount)
	{
		const UnicodeChar unicodeChar = utf8::decodeChar(&scanPtr);
		ret.push_back(unicodeChar);
	}

	return ret;
}
	
bool StringCell::operator==(const StringCell &other) const
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

int StringCell::compare(const StringCell *other) const
{
	std::uint32_t compareBytes = std::min(byteLength(), other->byteLength());

	// Bytewise comparisons in UTF-8 sort Unicode code points correctly
	int result = memcmp(constUtf8Data(), other->constUtf8Data(), compareBytes);

	if (result != 0)
	{
		return result;
	}
	else
	{
		return byteLength() - other->byteLength();
	}
}

int StringCell::compare(const StringCell *other, UnicodeChar (*converter)(UnicodeChar)) const
{
	const std::uint8_t *ourScanPtr = const_cast<StringCell*>(this)->utf8Data();
	const std::uint8_t *ourEndPtr = &constUtf8Data()[byteLength()];

	const std::uint8_t *theirScanPtr = const_cast<StringCell*>(other)->utf8Data();
	const std::uint8_t *theirEndPtr = &other->constUtf8Data()[other->byteLength()];

	while(true)
	{
		int ourEnd = ourScanPtr == ourEndPtr;
		int theirEnd = theirScanPtr == theirEndPtr;

		if (ourEnd || theirEnd)
		{
			// One of the strings ended
			return theirEnd - ourEnd;
		}

		const UnicodeChar ourChar = utf8::decodeChar(&ourScanPtr);
		const UnicodeChar theirChar = utf8::decodeChar(&theirScanPtr);

		int charCompare = ourChar.compare(theirChar, converter);

		if (charCompare != 0)
		{
			return charCompare;
		}
	}

	return 0;
}

BytevectorCell* StringCell::toUtf8Bytevector(World &world, std::int64_t start, std::int64_t end) 
{
	CharRange range = charRange(start, end);

	if (range.isNull())
	{
		return nullptr;
	}

	std::uint32_t newLength = range.endPointer - range.startPointer;
	SharedByteArray *byteArray;

	if ((newLength == byteLength()) && !dataIsInline())
	{
		// Reuse our existing byte array
		byteArray = static_cast<HeapStringCell*>(this)->heapByteArray()->ref();
	}
	else
	{
		// Create a new byte array and initialize it
		byteArray = SharedByteArray::createInstance(newLength);
		memcpy(byteArray->data(), range.startPointer, newLength);
	}

	return BytevectorCell::withByteArray(world, byteArray, newLength);
}

StringCell *StringCell::toConvertedString(World &world, UnicodeChar (*converter)(UnicodeChar))
{
	const std::uint8_t *scanPtr = utf8Data();
	const std::uint8_t *endPtr = &constUtf8Data()[byteLength()];

	StringCellBuilder builder(charLength());

	while(scanPtr != endPtr)
	{
		const UnicodeChar originalChar = utf8::decodeChar(&scanPtr);
		const UnicodeChar convertedChar = (*converter)(originalChar);

		builder << convertedChar;
	}

	return builder.result(world);
}

void StringCell::finalizeString()
{
	if (!dataIsInline())
	{
		static_cast<HeapStringCell*>(this)->heapByteArray()->unref();
	}
}

}
