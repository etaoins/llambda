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
	auto *newString = new std::uint8_t(byteLength + 1);
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
	auto newString = new std::uint8_t(byteLength + 1);

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
	auto newString = new std::uint8_t(totalByteLength + 1);
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
	
std::uint8_t* StringValue::charPointer(std::uint32_t offset)
{
	auto bytesLeft = byteLength();
	auto scanPtr = utf8Data();

	while(bytesLeft > 0)
	{
		if (!isContinuationByte(*scanPtr))
		{
			if (offset == 0)
			{
				return scanPtr;
			}
			
			offset--;
		}

		bytesLeft--;
		scanPtr++;
	}

	return nullptr;
}
	
StringValue::CodePoint StringValue::charAt(std::uint32_t offset)
{
	if (asciiOnlyHint())
	{
		if (offset >= byteLength())
		{
			return InvalidChar;
		}

		return utf8Data()[offset];
	}

	std::uint8_t* charPtr = charPointer(offset);

	if (charPtr == nullptr)
	{
		return InvalidChar;
	}

	return decodeUtf8Char(charPtr);
}

}
