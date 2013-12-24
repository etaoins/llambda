#include "StringCell.h"

#include <string.h>
#include <vector>
#include <algorithm>

#include "SymbolCell.h"
#include "BytevectorCell.h"

namespace
{
	using namespace lliby;

	// We will shrink our allocation if we would create more slack than this
	// Note this must fit in to StringLikeCell::m_allocSlackBytes
	const std::uint32_t MaximumAllocationSlack = 16 * 1024;

	// We round up allocations to this boundary
	// 
	// - Mac OS X 10.9/amd64 does small allocations in 16 byte multiples starting
	//   at 16 bytes
	//
	// - glibc 2.17/amd64 does small allocations in 16 byte multiples starting at
	//   24 bytes
	//
	// Rounding up to the next 8 byte binary will conservatively cover the above
	// two strategies without adding platform-specific logic 
	const std::uint32_t AllocationGranularity = 8;

	std::uint16_t suggestAllocSlackBytes(std::uint32_t minimumSize) 
	{
		std::uint32_t allocationOverrun = minimumSize % AllocationGranularity;

		if (allocationOverrun == 0)
		{
			return 0;
		}

		return AllocationGranularity - allocationOverrun;
	}

	const std::uint8_t ContinuationHeaderMask  = 0xC0;
	const std::uint8_t ContinuationHeaderValue = 0x80;

	const std::uint8_t TwoByteHeaderMask  = 0xE0;
	const std::uint8_t TwoByteHeaderValue = 0xC0;
	
	const std::uint8_t ThreeByteHeaderMask  = 0xF0;
	const std::uint8_t ThreeByteHeaderValue = 0xE0;
	
	const std::uint8_t FourByteHeaderMask  = 0xF8;
	const std::uint8_t FourByteHeaderValue = 0xF0;
	
	bool isContinuationByte(std::uint8_t byte)
	{
		// Any UTF-8 byte in the form 10xxxxxx is a continuation byte
		return (byte & ContinuationHeaderMask) == ContinuationHeaderValue;
	}

	UnicodeChar decodeUtf8Char(std::uint8_t **charPtr)
	{
		unsigned int continuationBytes;
		std::int32_t codePoint;

		std::uint8_t firstByte = *(*charPtr);

		if (firstByte <= 0x7f)
		{
			// This is ASCII, it's easy
			(*charPtr)++;

			return UnicodeChar(firstByte);
		}

		if ((firstByte & TwoByteHeaderMask) == TwoByteHeaderValue)
		{
			continuationBytes = 1;
			codePoint = firstByte & ~TwoByteHeaderMask;
		}
		else if ((firstByte & ThreeByteHeaderMask) == ThreeByteHeaderValue)
		{
			continuationBytes = 2;
			codePoint = firstByte & ~ThreeByteHeaderMask;
		}
		else if ((firstByte & FourByteHeaderMask) == FourByteHeaderValue)
		{
			continuationBytes = 3;
			codePoint = firstByte & ~FourByteHeaderMask;
		}
		else
		{
			return UnicodeChar();
		}

		while(continuationBytes--)
		{
			(*charPtr)++;

			// This will also catch running off the end via NULL
			if (!isContinuationByte(*(*charPtr)))
			{
				return UnicodeChar();
			}

			codePoint = (codePoint << 6) | (*(*charPtr) & ~ContinuationHeaderMask);
		}
			
		(*charPtr)++;
		return UnicodeChar(codePoint);
	}

	std::vector<std::uint8_t> encodeUtf8Char(UnicodeChar unicodeChar)
	{
		unsigned int continuationBytes;
		std::uint8_t firstByteHeader;
		std::int32_t codePoint = unicodeChar.codePoint();

		if (codePoint > 0x10FFFF)
		{
			// Not valid - see RFC-3629
			return std::vector<std::uint8_t>();
		}
		else if (codePoint >= 0x10000)
		{
			continuationBytes = 3;
			firstByteHeader = FourByteHeaderValue;
		}
		else if (codePoint >= 0x800)
		{
			continuationBytes = 2;
			firstByteHeader = ThreeByteHeaderValue;
		}
		else if (codePoint >= 0x80)
		{
			continuationBytes = 1;
			firstByteHeader = TwoByteHeaderValue;
		}
		else
		{
			// Pure ASCII
			return std::vector<std::uint8_t>{static_cast<std::uint8_t>(codePoint)};
		}

		std::vector<std::uint8_t> encoded(continuationBytes + 1);

		for(unsigned int i = continuationBytes; i; i--)
		{
			encoded[i] = ContinuationHeaderValue | (~ContinuationHeaderMask & codePoint);
			codePoint = codePoint >> 6;
		}

		encoded[0] = firstByteHeader | codePoint;

		return encoded;
	}
}

namespace lliby
{
	
StringCell* StringCell::createUninitialized(std::uint32_t byteLength)
{
	// We need 1 extra byte for the NULL terminator
	std::int32_t minimumSize = byteLength + 1;

	// Figure how much slack we should have
	std::int16_t allocSlackBytes = suggestAllocSlackBytes(minimumSize);

	auto utf8Data = new std::uint8_t[minimumSize + allocSlackBytes];
	
	return new StringCell(utf8Data, byteLength, 0, allocSlackBytes);
}

StringCell* StringCell::fromUtf8CString(const char *signedStr)
{
	std::uint64_t byteLength = 0;
	std::uint32_t charLength = 0;

	auto *str = reinterpret_cast<const std::uint8_t*>(signedStr);

	// Check for length in bytes and non-ASCII characters
	for(auto scanPtr = str;; scanPtr++)
	{
		if (*scanPtr == 0)
		{
			// We've reached the end of the string
			break;
		}

		if (!isContinuationByte(*scanPtr))
		{
			charLength++;
		}

		byteLength++;
	}

	if (byteLength > std::numeric_limits<std::uint32_t>::max())
	{
		return nullptr;
	}

	// Allocate the new string
	auto *newString = StringCell::createUninitialized(byteLength);

	// Initialize it
	newString->setCharLength(charLength);
	memcpy(newString->utf8Data(), str, byteLength + 1);

	return newString;
}
	
StringCell* StringCell::fromUtf8Data(const std::uint8_t *data, std::uint32_t byteLength)
{
	std::uint32_t charLength = 0;
	auto newString = StringCell::createUninitialized(byteLength);

	std::uint8_t *utf8Data = newString->utf8Data();

	// It seems that the cache friendliness of doing this in one loop likely
	// outweighs the faster copy we'd get from doing memcpy() at the end
	for(std::uint32_t i = 0; i < byteLength; i++)
	{
		if (!isContinuationByte(data[i]))
		{
			charLength++;
		}

		utf8Data[i] = data[i];
	}

	utf8Data[byteLength] = 0;

	// We know the character length now
	newString->setCharLength(charLength);

	return newString;
}
	
StringCell* StringCell::fromFill(std::uint32_t length, UnicodeChar fill)
{
	// Figure out how many bytes we'll need
	std::vector<std::uint8_t> encoded = encodeUtf8Char(fill);
	const size_t encodedCharSize = encoded.size();

	const std::uint32_t byteLength = encodedCharSize * length;

	// Allocate the string
	auto newString = StringCell::createUninitialized(byteLength);
	newString->setCharLength(length);

	std::uint8_t *utf8Data = newString->utf8Data();

	// Actually fill
	for(std::uint32_t i = 0; i < length; i++) 
	{
		memcpy(&utf8Data[i * encodedCharSize], encoded.data(), encodedCharSize);
	}

	// NULL terminate
	utf8Data[encodedCharSize * length] = 0;

	return newString;
}
	
StringCell* StringCell::fromAppended(const std::list<const StringCell*> &strings)
{
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

	// Allocate the new string and null terminate it
	auto newString = StringCell::createUninitialized(totalByteLength);
	newString->setCharLength(totalCharLength);

	std::uint8_t *copyPtr = newString->utf8Data();

	// Copy all the string parts over
	for(auto stringPart : strings)
	{
		memcpy(copyPtr, stringPart->utf8Data(), stringPart->byteLength());
		copyPtr += stringPart->byteLength();
	}
	
	copyPtr[0] = 0;

	return newString;
}
	
StringCell* StringCell::fromUnicodeChars(const std::list<UnicodeChar> &unicodeChars)
{
	std::vector<std::uint8_t> encodedData;
	std::uint32_t charLength = 0;

	// The encoded data will have the be at least this size
	encodedData.reserve(unicodeChars.size());

	for(auto unicodeChar : unicodeChars)
	{
		const std::vector<std::uint8_t> encodedChar = encodeUtf8Char(unicodeChar);
		encodedData.insert(encodedData.end(), encodedChar.begin(), encodedChar.end());

		charLength++;
	}
	
	const std::uint32_t totalByteLength = encodedData.size();

	// Create a new string to write in to
	auto newString = StringCell::createUninitialized(totalByteLength);
	newString->setCharLength(charLength);

	std::uint8_t *utf8Data = newString->utf8Data();

	memcpy(utf8Data, encodedData.data(), totalByteLength);

	utf8Data[totalByteLength] = 0;

	return newString;
}
	
std::uint8_t* StringCell::charPointer(std::uint32_t charOffset) const
{
	return charPointer(utf8Data(), byteLength(), charOffset);
}

std::uint8_t* StringCell::charPointer(std::uint8_t *scanFrom, std::uint32_t bytesLeft, uint32_t charOffset) const
{
	if (asciiOnly())
	{
		if (charOffset >= bytesLeft)
		{
			return nullptr;
		}

		return &scanFrom[charOffset];
	}

	while(bytesLeft > 0)
	{
		if (!isContinuationByte(*scanFrom))
		{
			if (charOffset == 0)
			{
				return scanFrom;
			}
			
			charOffset--;
		}

		bytesLeft--;
		scanFrom++;
	}

	return nullptr;
}
	
StringCell::CharRange StringCell::charRange(std::int64_t start, std::int64_t end) const
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
		// Combined with the above logic that means the start must also be
		// less than or equal to the length
		return CharRange { 0 };
	}

	std::uint8_t *startPointer;
	
	if (start == charLength())
	{
		// The string scan below will fail because we want the pointer just
		// past the last character. Note this is only possible if:
		// start == end == charLength()
		startPointer = &utf8Data()[byteLength()];
	}
	else
	{
		startPointer = charPointer(start);

		if (startPointer == nullptr)
		{
			// Fell off the end looking for the start
			// This means we have corrupted UTF-8 data because the checks above
			// should have caught that.
			return CharRange { 0 };
		}
	}

	const std::uint32_t charCount = end - start;
	std::uint8_t *endPointer;
	
	if (end == charLength())
	{
		endPointer = &utf8Data()[byteLength()];
	}
	else
	{
		// Find our end pointer
		const std::uint32_t bytesLeft = byteLength() - (startPointer - utf8Data());
		endPointer = charPointer(startPointer, bytesLeft, charCount);

		if (endPointer == nullptr)
		{
			// Fell off the end
			// This means we have corrupted UTF-8 data
			return CharRange { 0 };
		}
	}

	return CharRange { startPointer, endPointer, charCount };
}
	
UnicodeChar StringCell::charAt(std::uint32_t offset) const
{
	std::uint8_t* charPtr = charPointer(offset);

	if (charPtr == nullptr)
	{
		return UnicodeChar();
	}

	return decodeUtf8Char(&charPtr);
}
	
bool StringCell::replaceBytes(const CharRange &range, std::uint8_t *pattern, unsigned int patternBytes, unsigned int count)
{
	const unsigned int requiredBytes = patternBytes * count;
	const unsigned int replacedBytes = range.byteCount();
	
	if (requiredBytes == replacedBytes)
	{
		std::uint8_t *copyDest = range.startPointer;
		while(count--)
		{
			memcpy(copyDest, pattern, patternBytes);
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
		// Include the NULL terminator
		const std::uint32_t finalBytes = newByteLength + 1 - initialBytes - requiredBytes;
		
		// Figure out how much slack our allocation would have after this	
		const std::int64_t newAllocSlackBytes = static_cast<std::int64_t>(byteLength()) - newByteLength + allocSlackBytes();

		// Make sure we have enough space and we don't exceed our maximum allocation size
		const bool needRealloc = (newAllocSlackBytes < 0) ||
			                      (newAllocSlackBytes > MaximumAllocationSlack);

		std::uint8_t* destString;
		
		if (needRealloc)
		{
			std::uint32_t newSlackBytes = suggestAllocSlackBytes(newByteLength);

			// Create our new destination string
			destString = new std::uint8_t[newByteLength + 1 + newSlackBytes];

			// Update our slack byte count
			setAllocSlackBytes(newSlackBytes);

			// Fill the initial chunk of the string
			memcpy(destString, utf8Data(), initialBytes); 
		}
		else
		{
			setAllocSlackBytes(newAllocSlackBytes);
			destString = utf8Data();

			// The initial chunk is already correct
		}
		
		// Move the unchanged chunk at the end
		// We need to do this now because if the pattern bytes are longer than the
		// byte we're replacing then we might overwrite the beginning of the
		// unchanged chunk 
		memmove(destString + initialBytes + requiredBytes, range.startPointer + replacedBytes, finalBytes);
		
		std::uint8_t* copyDest = destString + initialBytes;

		while(count--)
		{
			// memmove because ::replace() can copy a substring to itself
			memmove(copyDest, pattern, patternBytes);
			copyDest += patternBytes;
		}

		// Update ourselves with our new string
		setByteLength(newByteLength);

		if (needRealloc)
		{
			delete[] utf8Data();
			setUtf8Data(destString);
		}
	}
	
	return true;
}
	
bool StringCell::fill(UnicodeChar unicodeChar, std::int64_t start, std::int64_t end)
{
	CharRange range = charRange(start, end);

	if (range.isNull())
	{
		// Invalid range
		return false;
	}

	// Encode the new character
	std::vector<std::uint8_t> encoded = encodeUtf8Char(unicodeChar);

	if (encoded.size() == 0)
	{
		// Invalid code point
		return false;
	}

	return replaceBytes(range, encoded.data(), encoded.size(), range.charCount);
}
	
bool StringCell::replace(std::uint32_t offset, const StringCell *from, std::int64_t fromStart, std::int64_t fromEnd)
{
	CharRange fromRange = from->charRange(fromStart, fromEnd);

	if (fromRange.isNull())
	{
		return false;
	}

	CharRange toRange = charRange(offset, offset + fromRange.charCount);

	if (toRange.isNull() || (toRange.charCount != fromRange.charCount))
	{
		return false;
	}
	
	return replaceBytes(toRange, fromRange.startPointer, fromRange.byteCount());
}
	
bool StringCell::setCharAt(std::uint32_t offset, UnicodeChar unicodeChar)
{
	return fill(unicodeChar, offset, offset + 1);
}
	
StringCell* StringCell::copy(std::int64_t start, std::int64_t end) const
{
	CharRange range = charRange(start, end);

	if (range.isNull())
	{
		// Invalid range
		return nullptr;
	}

	const std::uint32_t newByteLength = range.byteCount();

	// Create the new string
	auto newString = StringCell::createUninitialized(newByteLength);
	newString->setCharLength(range.charCount);

	std::uint8_t *newUtf8Data = newString->utf8Data();

	memcpy(newUtf8Data, range.startPointer, newByteLength);
	newUtf8Data[newByteLength] = 0;

	return newString;
}
	
std::list<UnicodeChar> StringCell::unicodeChars(std::int64_t start, std::int64_t end) const
{
	if ((end != -1) && (end < start))
	{
		// Doesn't make sense
		return std::list<UnicodeChar>();
	}

	std::uint8_t *scanPtr = charPointer(start);
	std::uint8_t *endPtr = &utf8Data()[byteLength()];

	if (scanPtr == nullptr)
	{
		return std::list<UnicodeChar>();
	}

	// It doesn't look like std::list::size() is O(1)
	unsigned int addedChars = 0;
	std::list<UnicodeChar> ret;

	while(scanPtr < endPtr)
	{
		const UnicodeChar unicodeChar = decodeUtf8Char(&scanPtr);

		if (!unicodeChar.isValid())
		{
			return std::list<UnicodeChar>();
		}

		ret.push_back(unicodeChar);

		addedChars++;

		if ((end != -1) && (addedChars == (end - start)))
		{
			// We have enough characters
			return ret;
		}
	}
		
	if (end != -1)
	{
		// We fell off the end
		return std::list<UnicodeChar>();
	}

	return ret;
}

int StringCell::compareCaseSensitive(const StringCell *other) const
{
	std::uint32_t compareBytes = std::min(byteLength(), other->byteLength());

	// Bytewise comparisons in UTF-8 sort Unicode code points correctly
	int result = memcmp(utf8Data(), other->utf8Data(), compareBytes);

	if (result != 0)
	{
		return result;
	}
	else
	{
		return byteLength() - other->byteLength();
	}
}

int StringCell::compareCaseInsensitive(const StringCell *other) const
{
	std::uint8_t *ourScanPtr = utf8Data();
	std::uint8_t *ourEndPtr = &utf8Data()[byteLength()];

	std::uint8_t *theirScanPtr = other->utf8Data();
	std::uint8_t *theirEndPtr = &other->utf8Data()[other->byteLength()];
	
	while(true)
	{
		const int ourEnd = ourScanPtr >= ourEndPtr;
		const int theirEnd = theirScanPtr >= theirEndPtr;

		if (ourEnd || theirEnd)
		{
			return theirEnd - ourEnd;
		}

		const UnicodeChar ourChar = decodeUtf8Char(&ourScanPtr);
		if (!ourChar.isValid())
		{
			// Pretend we ended early
			return -1;
		}
		
		const UnicodeChar theirChar = decodeUtf8Char(&theirScanPtr);
		if (!theirChar.isValid())
		{
			// Pretend they ended early
			return 1;
		}

		int charCompare = ourChar.compare(theirChar, CaseSensitivity::Insensitive);

		if (charCompare != 0)
		{
			return charCompare;
		}
	}

	return 0;
}

int StringCell::compare(const StringCell *other, CaseSensitivity cs) const
{
	if (cs == CaseSensitivity::Sensitive)
	{
		return compareCaseSensitive(other);
	}
	else
	{
		return compareCaseInsensitive(other);
	}
}
	
SymbolCell* StringCell::toSymbol() const
{
	// This is easy, just copy our UTF-8 data
	auto newString = new std::uint8_t[byteLength() + 1];
	memcpy(newString, utf8Data(), byteLength() + 1);

	return new SymbolCell(newString, byteLength(), charLength());
}
	
BytevectorCell* StringCell::toUtf8Bytevector(std::int64_t start, std::int64_t end) const
{
	CharRange range = charRange(start, end);

	if (range.isNull())
	{
		return nullptr;
	}

	std::uint32_t newLength = range.endPointer - range.startPointer;
	auto *newData = new std::uint8_t[newLength];

	memcpy(newData, range.startPointer, newLength);
	return new BytevectorCell(newData, newLength);
}
	
StringCell *StringCell::toConvertedString(UnicodeChar (UnicodeChar::* converter)() const) const
{
	std::vector<std::uint8_t> convertedData;

	// Guess that our converted data will be about the same size as the original
	// data. Case conversion rarely moves code points far from their original
	// value
	convertedData.reserve(byteLength());

	std::uint8_t *scanPtr = utf8Data();
	const std::uint8_t *endPtr = &utf8Data()[byteLength()];

	while(scanPtr < endPtr)
	{
		const UnicodeChar originalChar = decodeUtf8Char(&scanPtr);

		if (!originalChar.isValid())
		{
			// Invalid UTF-8 encoding
			return nullptr;
		}

		const UnicodeChar convertedChar = (originalChar.*converter)();

		const std::vector<std::uint8_t> encodedChar = encodeUtf8Char(convertedChar);
		convertedData.insert(convertedData.end(), encodedChar.begin(), encodedChar.end());
	}
	
	const std::uint32_t totalByteLength = convertedData.size();

	auto newString = new std::uint8_t[totalByteLength + 1];
	memcpy(newString, convertedData.data(), totalByteLength);

	newString[totalByteLength] = 0;

	return new StringCell(newString, totalByteLength, charLength());
}

StringCell* StringCell::toUppercaseString() const
{
	return toConvertedString(&UnicodeChar::toUppercase);
}

StringCell* StringCell::toLowercaseString() const
{
	return toConvertedString(&UnicodeChar::toLowercase);
}

StringCell* StringCell::toCaseFoldedString() const
{
	return toConvertedString(&UnicodeChar::toCaseFolded);
}

}
