#ifndef _LLIBY_UNICODE_UTF8_H
#define _LLIBY_UNICODE_UTF8_H

#include <cstdint>
#include <vector>
#include <cassert>
#include <iterator>

#include "unicode/UnicodeChar.h"

namespace lliby
{
namespace utf8
{

static const std::uint8_t ContinuationHeaderMask  = 0xC0;
static const std::uint8_t ContinuationHeaderValue = 0x80;

static const std::uint8_t TwoByteHeaderMask  = 0xE0;
static const std::uint8_t TwoByteHeaderValue = 0xC0;

static const std::uint8_t ThreeByteHeaderMask  = 0xF0;
static const std::uint8_t ThreeByteHeaderValue = 0xE0;

static const std::uint8_t FourByteHeaderMask  = 0xF8;
static const std::uint8_t FourByteHeaderValue = 0xF0;

inline bool isContinuationByte(std::uint8_t byte)
{
	// Any UTF-8 byte in the form 10xxxxxx is a continuation byte
	return (byte & ContinuationHeaderMask) == ContinuationHeaderValue;
}

inline UnicodeChar decodeUtf8Char(std::uint8_t **charPtr)
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

		// decodeUtf8Char() should only be called on pre-verified UTF-8 input
		// This shouldn't happen but it's an easy sanity check to make
		assert(isContinuationByte(*(*charPtr)));

		codePoint = (codePoint << 6) | (*(*charPtr) & ~ContinuationHeaderMask);
	}
		
	(*charPtr)++;
	return UnicodeChar(codePoint);
}

template<typename T>
T appendUtf8Char(UnicodeChar unicodeChar, T output)
{
	std::int32_t codePoint = unicodeChar.codePoint();

	if (codePoint < 0x80)
	{
		// Pure ASCII
		*(output++) = codePoint;
	}
	else if (codePoint < 0x800)
	{
		*(output++) = (codePoint >> 6)  | TwoByteHeaderValue;
		*(output++) = (codePoint        & ~ContinuationHeaderMask) | ContinuationHeaderValue;
	}
	else if (codePoint < 0x10000)
	{
		*(output++) = (codePoint >> 12) | ThreeByteHeaderValue;
		*(output++) = ((codePoint >> 6) & ~ContinuationHeaderMask)  | ContinuationHeaderValue;
		*(output++) = (codePoint         & ~ContinuationHeaderMask) | ContinuationHeaderValue;
	}
	else if (codePoint < 0x110000)
	{
		*(output++) = (codePoint >> 18)  | FourByteHeaderValue;
		*(output++) = ((codePoint >> 12) & ~ContinuationHeaderMask) | ContinuationHeaderValue;
		*(output++) = ((codePoint >> 6)  & ~ContinuationHeaderMask) | ContinuationHeaderValue;
		*(output++) = (codePoint         & ~ContinuationHeaderMask) | ContinuationHeaderValue;
	}

	return output;
}

inline std::vector<std::uint8_t> encodeUtf8Char(UnicodeChar unicodeChar)
{
	std::vector<uint8_t> encodedChar;
	appendUtf8Char(unicodeChar, std::back_inserter(encodedChar));
	return encodedChar;
}

}
}

#endif
