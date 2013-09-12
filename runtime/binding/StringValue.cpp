#include "StringValue.h"

#include <string.h>
#include <vector>

namespace
{
	using namespace lliby;

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

	StringValue::CodePoint decodeUtf8Char(const std::uint8_t *charPtr, unsigned int *length = nullptr)
	{
		unsigned int continuationBytes;
		StringValue::CodePoint codePoint;

		std::uint8_t firstByte = *charPtr;

		if (firstByte <= 0x7f)
		{
			// This is ASCII, it's easy
			if (length)
			{
				*length = 1;
			}

			return firstByte;
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
			return StringValue::InvalidChar;
		}

		if (length)
		{
			*length = continuationBytes + 1;
		}

		while(continuationBytes--)
		{
			charPtr++;

			// This will also catch running off the end via NULL
			if (!isContinuationByte(*charPtr))
			{
				return StringValue::InvalidChar;
			}

			codePoint = (codePoint << 6) | (*charPtr & ~ContinuationHeaderMask);
		}

		return codePoint;
	}

	std::vector<std::uint8_t> encodeUtf8Char(StringValue::CodePoint codePoint)
	{
		unsigned int continuationBytes;
		std::uint8_t firstByteHeader;

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

StringValue* StringValue::fromUtf8CString(const char *signedStr)
{
	std::uint32_t byteLength = 0;
	bool asciiOnly = true;

	auto *str = reinterpret_cast<const std::uint8_t*>(signedStr);

	// Check for length in bytes and non-ASCII characters
	for(auto scanPtr = str;; scanPtr++)
	{
		if (*scanPtr == 0)
		{
			// We've reached the end of the string
			break;
		}

		if (*scanPtr > 0x7f)
		{
			asciiOnly = false;
		}

		byteLength++;
	}

	// Allocate the new string room for the NULL terminator
	auto *newString = new std::uint8_t[byteLength + 1];
	memcpy(newString, str, byteLength + 1);

	return new StringValue(newString, byteLength, asciiOnly);
}
	
StringValue* StringValue::fromFill(std::uint32_t length, CodePoint fill)
{
	// Figure out how many bytes we'll need
	std::vector<std::uint8_t> encoded = encodeUtf8Char(fill);
	const size_t encodedCharSize = encoded.size();

	const std::uint32_t byteLength = encodedCharSize * length;
	const bool asciiOnly = (encodedCharSize == 1) || (length == 0);

	// Allocate the string
	auto newString = new std::uint8_t[byteLength + 1];

	// Actually fill
	for(std::uint32_t i = 0; i < length; i++) 
	{
		memcpy(&newString[i * encodedCharSize], encoded.data(), encodedCharSize);
	}

	// NULL terminate
	newString[encodedCharSize * length] = 0;

	return new StringValue(newString, byteLength, asciiOnly);
}
	
StringValue* StringValue::fromAppended(const std::list<const StringValue*> &strings)
{
	std::uint32_t totalByteLength = 0;
	bool asciiOnly = true;

	for(auto stringPart : strings)
	{
		totalByteLength += stringPart->byteLength();
		
		if (!stringPart->asciiOnlyHint())
		{
			asciiOnly = false;
		}
	}

	// Allocate the new string and null terminate it
	auto newString = new std::uint8_t[totalByteLength + 1];
	newString[totalByteLength] = 0;

	// Copy all the string parts over
	auto copyPtr = newString;
	
	for(auto stringPart : strings)
	{
		memcpy(copyPtr, stringPart->utf8Data(), stringPart->byteLength());
		copyPtr += stringPart->byteLength();
	}

	return new StringValue(newString, totalByteLength, asciiOnly);
}
	
StringValue* StringValue::fromCodePoints(const std::list<CodePoint> &codePoints)
{
	std::vector<std::uint8_t> encodedData;
	bool asciiOnly = true;

	// The encoded data will have the be at least this size
	encodedData.reserve(codePoints.size());

	for(auto codePoint : codePoints)
	{
		const std::vector<std::uint8_t> encodedChar = encodeUtf8Char(codePoint);
		encodedData.insert(encodedData.end(), encodedChar.begin(), encodedChar.end());

		if (codePoint > 0x7f)
		{
			asciiOnly = false;
		}
	}
	
	const std::uint32_t totalByteLength = encodedData.size();
	auto newString = new std::uint8_t[totalByteLength + 1];
	memcpy(newString, encodedData.data(), totalByteLength);

	newString[totalByteLength] = 0;

	return new StringValue(newString, totalByteLength, asciiOnly);
}
	
std::uint32_t StringValue::charLength() const
{
	if (asciiOnlyHint())
	{
		// Easy
		return byteLength();
	}

	std::uint32_t length = 0;

	for(std::uint32_t i = 0; i < byteLength(); i++)
	{
		// Count everything except continuation bytes
		if (!isContinuationByte(utf8Data()[i]))
		{
			length++;
		}
	}

	return length;
}
	
std::uint8_t* StringValue::charPointer(std::uint32_t charOffset) const
{
	return charPointer(utf8Data(), byteLength(), charOffset);
}

std::uint8_t* StringValue::charPointer(std::uint8_t *scanFrom, std::uint32_t bytesLeft, uint32_t charOffset) const
{
	if (asciiOnlyHint())
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
	
StringValue::CodePoint StringValue::charAt(std::uint32_t offset) const
{
	std::uint8_t* charPtr = charPointer(offset);

	if (charPtr == nullptr)
	{
		return InvalidChar;
	}

	return decodeUtf8Char(charPtr);
}
	
bool StringValue::fill(CodePoint codePoint, std::int64_t start, std::int64_t end)
{
	if ((end != -1) && (end < start))
	{
		// Doesn't make sense
		return false;
	}

	std::uint8_t *startPointer = charPointer(start);

	if (startPointer == nullptr)
	{
		// Fell off the end
		return false;
	}
		
	std::uint32_t charCount;
	std::uint8_t *endPointer;
	
	if (end != -1)
	{
		// We know the exact number of characters
		charCount = end - start + 1;
		
		// Find the end pointer
		const std::uint32_t bytesLeft = byteLength() - (startPointer - utf8Data());
		endPointer = charPointer(startPointer, bytesLeft, charCount - 1);

		if (endPointer == nullptr)
		{
			// Fell off the end
			return false;
		}

		// Read past the last character
		unsigned int lastCharLength;
		
		if (decodeUtf8Char(endPointer, &lastCharLength) == StringValue::InvalidChar)
		{
			// We have corrupted UTF-8 internally (!)
			return false;
		}

		endPointer += lastCharLength; 
	}
	else 
	{
		endPointer = &utf8Data()[byteLength()];

		if (asciiOnlyHint())
		{
			// We can cheat here if we're ASCII
			charCount = byteLength() - start;
		}
		else
		{
			// Count out the characters
			charCount = 0;

			for(auto scanPtr = startPointer; scanPtr < endPointer; scanPtr++)
			{
				if (!isContinuationByte(*scanPtr))
				{
					charCount++;
				}
			}
		}
	}

	// Encode the new character
	std::vector<std::uint8_t> encoded = encodeUtf8Char(codePoint);

	if (encoded.size() == 0)
	{
		// Invalid code point
		return false;
	}
	
	// Now that we know we'll succeed clear our ASCII-only hint
	if (codePoint > 0x7f)
	{
		// We're not ASCII anymore
		setAsciiOnlyHint(false);
	}
	else if (((startPointer - utf8Data()) == start) &&
			 (endPointer == &utf8Data()[byteLength()]))
	{
		// Everything before us was ASCII, we're ASCII, and we're filling to end
		setAsciiOnlyHint(true);
	}
	
	const unsigned int requiredBytes = encoded.size() * charCount;
	const unsigned int replacedBytes = endPointer - startPointer;
	
	if (requiredBytes == replacedBytes)
	{
		while(charCount--)
		{
			memcpy(startPointer, encoded.data(), encoded.size());
			startPointer += encoded.size();
		}
	}
	else
	{
		// Create a new string from pieces of the old string
		const std::uint32_t newByteLength = byteLength() + requiredBytes - replacedBytes;
		const std::uint32_t initialBytes = startPointer - utf8Data(); 
		// Include the NULL terminator
		const std::uint32_t finalBytes = newByteLength + 1 - initialBytes - requiredBytes;
		
		const bool needRealloc = newByteLength > byteLength();

		std::uint8_t* destString;
		
		if (needRealloc)
		{
			destString = new std::uint8_t[newByteLength + 1];
			// Fill the initial chunk of the string
			memcpy(destString, utf8Data(), initialBytes); 
		}
		else
		{
			destString = utf8Data();
			// The initial chunk is already correct
		}
		
		std::uint8_t* copyDest = destString + initialBytes;

		while(charCount--)
		{
			memcpy(copyDest, encoded.data(), encoded.size());
			copyDest += encoded.size();
		}

		memmove(copyDest, startPointer + replacedBytes, finalBytes);

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
	
bool StringValue::setCharAt(std::uint32_t offset, CodePoint codePoint)
{
	return fill(codePoint, offset, offset);
}
	
StringValue* StringValue::copy(std::int64_t start, std::int64_t end)
{
	if ((end != -1) && (end < start))
	{
		// Doesn't make sense
		return nullptr;
	}
	
	std::uint8_t *startPointer = charPointer(start);
	std::uint8_t *endPointer;
	bool asciiOnly;

	if (end == -1)
	{
		// This is easy
		endPointer = &utf8Data()[byteLength()];
		asciiOnly = asciiOnlyHint();
	}
	else
	{
		const std::uint32_t bytesLeft = byteLength() - (startPointer - utf8Data());
		const std::uint32_t charLength = end - start + 1;
		
		// This gets a pointer to the beginning of the character
		endPointer = charPointer(startPointer, bytesLeft, charLength - 1);

		if (endPointer == nullptr)
		{
			// Off the end
			return nullptr;
		}

		// Find the length of the character in bytes
		unsigned int length;
		if (decodeUtf8Char(endPointer, &length) == StringValue::InvalidChar)
		{
			return nullptr;
		}

		// Include it
		endPointer += length;

		// We scanned the entire substring so we get this for free
		asciiOnly = charLength == (endPointer - startPointer);
	}

	const std::uint32_t newByteLength = endPointer - startPointer;

	// Create the new string
	auto newString = new std::uint8_t[newByteLength + 1];
	memcpy(newString, startPointer, newByteLength);
	newString[newByteLength] = 0;

	return new StringValue(newString, newByteLength, asciiOnly);
}
	
std::list<StringValue::CodePoint> StringValue::codePoints(std::int64_t start, std::int64_t end) const
{
	if ((end != -1) && (end < start))
	{
		// Doesn't make sense
		return std::list<CodePoint>();
	}

	std::uint8_t *scanPtr = charPointer(start);
	std::uint8_t *endPtr = &utf8Data()[byteLength()];

	if (scanPtr == nullptr)
	{
		return std::list<CodePoint>();
	}

	// It doesn't look like std::list::size() is O(1)
	unsigned int addedChars = 0;
	std::list<CodePoint> ret;

	while(scanPtr < endPtr)
	{
		unsigned int charLength;

		CodePoint codePoint = decodeUtf8Char(scanPtr, &charLength);

		if (codePoint == InvalidChar)
		{
			return std::list<CodePoint>();
		}

		ret.push_back(codePoint);

		scanPtr += charLength;
		addedChars++;

		if ((end != -1) && (addedChars == (end - start + 1)))
		{
			// We have enough characters
			return ret;
		}
	}
		
	if (end != -1)
	{
		// We fell off the end
		return std::list<CodePoint>();
	}

	return ret;
}

}
