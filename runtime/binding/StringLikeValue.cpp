#include "StringLikeValue.h"

#include <string.h>

namespace
{
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
}

namespace lliby
{

StringLikeValue* StringLikeValue::fromUtf8CString(BoxedTypeId typeId, const char *signedStr)
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
	std::uint8_t *newString = new std::uint8_t(byteLength + 1);
	memcpy(newString, str, byteLength + 1);

	return new StringLikeValue(typeId, newString, byteLength, asciiOnly);
}
	
std::uint32_t StringLikeValue::charLength() const
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
	
std::uint8_t* StringLikeValue::charPointer(std::uint32_t offset)
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
	
std::int32_t StringLikeValue::charAt(std::uint32_t offset)
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

	std::uint8_t firstByte = *charPtr;

	if (firstByte <= 0x7f)
	{
		// This is ASCII, it's easy
		return firstByte;
	}

	unsigned int continuationBytes;
	std::int32_t codePoint;

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
		return InvalidChar;
	}

	while(continuationBytes--)
	{
		charPtr++;

		// This will also catch running off the end via NULL
		if (!isContinuationByte(*charPtr))
		{
			return InvalidChar;
		}

		codePoint = (codePoint << 6) | (*charPtr & ~ContinuationHeaderMask);
	}

	return codePoint;
}
	
bool StringLikeValue::operator==(const StringLikeValue &other)
{
	if (byteLength() != other.byteLength())
	{
		return false;
	}

	// Don't depend on asciiOnlyHint() here as it's only a hint
	// For example, our ASCII strings can be mutated to Unicode and back 
	// without regaining their ASCII-only hint
	
	return memcmp(utf8Data(), other.utf8Data(), byteLength()) == 0;
}
	
void StringLikeValue::finalize()
{
	delete m_utf8Data;
}

}
