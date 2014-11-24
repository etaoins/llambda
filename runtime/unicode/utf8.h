#ifndef _LLIBY_UNICODE_UTF8_H
#define _LLIBY_UNICODE_UTF8_H

#include <cstdint>
#include <vector>
#include <cassert>

#include "unicode/UnicodeChar.h"

namespace lliby
{
namespace utf8
{

static const int LongestByteSequence = 4;

static const std::uint8_t ContinuationHeaderMask  = 0xC0;
static const std::uint8_t ContinuationHeaderValue = 0x80;

static const std::uint8_t TwoByteHeaderValue   = 0xC0;
static const std::uint8_t ThreeByteHeaderValue = 0xE0;
static const std::uint8_t FourByteHeaderValue  = 0xF0;
// Note that five byte sequences are no longer valid UTF-8. This is used just for range checking.
static const std::uint8_t FiveByteHeaderValue  = 0xF8;

inline bool isContinuationByte(std::uint8_t byte)
{
	// Any UTF-8 byte in the form 10xxxxxx is a continuation byte
	return (byte & ContinuationHeaderMask) == ContinuationHeaderValue;
}

/**
 * Returns the number of bytes in the UTF-8 sequence starting with the passed byte
 *
 * Valid UTF-8 sequences are between 1 and 4 bytes long. If the byte isn't recognized as the first byte in a valid UTF-8
 * sequence -1 will be returned.
 */
inline int bytesInSequence(std::uint8_t firstByte)
{
	if (firstByte < 0x80)
	{
		return 1;
	}
	else if (firstByte < ThreeByteHeaderValue)
	{
		return 2;
	}
	else if (firstByte < FourByteHeaderValue)
	{
		return 3;
	}
	else if (firstByte < FiveByteHeaderValue)
	{
		return 4;
	}

	return -1;
}

/**
 * Returns the numbers of bytes needed to encode the passed Unicode character
 *
 * Valid UTF-8 code points require between 1 and 4 bytes to encode. If the code point is outside the range of valid
 * Unicode code points then -1 is returned
 */
inline int bytesForChar(UnicodeChar unicodeChar)
{
	const std::int32_t codePoint = unicodeChar.codePoint();

	if (codePoint < UnicodeChar::FirstCodePoint)
	{
		return -1;
	}
	else if (codePoint < 0x80)
	{
		return 1;
	}
	else if (codePoint < 0x800)
	{
		return 2;
	}
	else if (codePoint < 0x10000)
	{
		return 3;
	}
	else if (codePoint <= UnicodeChar::LastCodePoint)
	{
		return 4;
	}

	return -1;
}

/**
 * Validates UTF-8 encoded data in the given range
 *
 * This will either return the number of characters in the sequence or throw an InvalidByteSequenceException
 */
std::size_t validateData(const std::uint8_t *start, const std::uint8_t *end);

/**
 * Counts the number of characters in validated UTF-8 data
 *
 * This behaves the same as validateData except invalid UTF-8 will result in an undefined result instead of throwing
 * an exception
 */
inline std::size_t countChars(const std::uint8_t *start, const std::uint8_t *end)
{
	std::size_t charCount = 0;

	for(auto scanPtr = start; scanPtr != end; scanPtr++)
	{
		if (!isContinuationByte(*scanPtr))
		{
			charCount++;
		}
	}

	return charCount;
}

/**
 * Decodes a UTF-8 sequence from validated UTF-8 data
 *
 * @param  charIt  Beginning of the UTF-8 sequence. This will be advanced to the byte following the end of the UTF-8
 *                 sequence.
 *
 * @return Decoded Unicode character
 */
inline UnicodeChar decodeChar(const std::uint8_t **charIt)
{
	const int seqBytes = bytesInSequence(*(*charIt));

	std::int32_t codePoint;

	switch(seqBytes)
	{
	case 1:
		codePoint = *(*charIt)++;
		break;
	case 2:
		codePoint = ((*(*charIt)++ & ~TwoByteHeaderValue) << 6) |
			         (*(*charIt)++ & ~ContinuationHeaderMask);
		break;
	case 3:
		codePoint = ((*(*charIt)++ & ~ThreeByteHeaderValue) << 12) |
			        ((*(*charIt)++ & ~ContinuationHeaderMask) << 6) |
			        ((*(*charIt)++ & ~ContinuationHeaderMask));
		break;
	case 4:
		codePoint = ((*(*charIt)++ & ~FourByteHeaderValue) << 18) |
			        ((*(*charIt)++ & ~ContinuationHeaderMask) << 12) |
			        ((*(*charIt)++ & ~ContinuationHeaderMask) << 6) |
			        ((*(*charIt)++ & ~ContinuationHeaderMask));
		break;
	default:
		assert(false);
		return UnicodeChar();
	}

	return UnicodeChar(codePoint);
}

template<typename T>
T appendChar(UnicodeChar unicodeChar, T output)
{
	std::int32_t codePoint = unicodeChar.codePoint();
	const int seqBytes = bytesForChar(unicodeChar);

	switch(seqBytes)
	{
	case 1:
		// Pure ASCII
		*(output++) = codePoint;
		break;
	case 2:
		*(output++) = (codePoint >> 6)  | TwoByteHeaderValue;
		*(output++) = (codePoint        & ~ContinuationHeaderMask) | ContinuationHeaderValue;
		break;
	case 3:
		*(output++) = (codePoint >> 12) | ThreeByteHeaderValue;
		*(output++) = ((codePoint >> 6) & ~ContinuationHeaderMask) | ContinuationHeaderValue;
		*(output++) = (codePoint        & ~ContinuationHeaderMask) | ContinuationHeaderValue;
		break;
	case 4:
		*(output++) = (codePoint >> 18)  | FourByteHeaderValue;
		*(output++) = ((codePoint >> 12) & ~ContinuationHeaderMask) | ContinuationHeaderValue;
		*(output++) = ((codePoint >> 6)  & ~ContinuationHeaderMask) | ContinuationHeaderValue;
		*(output++) = (codePoint         & ~ContinuationHeaderMask) | ContinuationHeaderValue;
		break;
	default:
		// Invalid code point!
		assert(false);
		break;
	}

	return output;
}

inline std::vector<std::uint8_t> encodeChar(UnicodeChar unicodeChar)
{
	std::vector<uint8_t> encodedChar;
	appendChar(unicodeChar, std::back_inserter(encodedChar));
	return encodedChar;
}

}
}

#endif
