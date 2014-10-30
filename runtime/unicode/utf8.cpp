#include "utf8.h"

#include "utf8/InvalidByteSequenceException.h"

namespace lliby
{
namespace utf8
{

namespace
{
	const std::uint8_t* validateChar(const std::uint8_t *start, const std::uint8_t *end)
	{
		const std::size_t inputBytes = end - start;

		const std::uint8_t *scanPtr = start;
		std::uint8_t firstByte = *(scanPtr++);
		const int seqBytes = bytesInSequence(firstByte);

		unsigned int continuationBytes;
		std::int32_t codePoint;

		if (seqBytes < 1)
		{
			// Invalid header byte
			throw InvalidByteSequenceException(0, 0, "Invalid header byte");
		}

		if (inputBytes < seqBytes)
		{
			// Not enough input
			throw InvalidByteSequenceException(0, inputBytes - 1, "Truncated input");
		}

		switch(seqBytes)
		{
		case 1:
			return scanPtr;
		case 2:
			continuationBytes = 1;
			codePoint = firstByte & ~TwoByteHeaderMask;
			break;
		case 3:
			continuationBytes = 2;
			codePoint = firstByte & ~ThreeByteHeaderMask;
			break;
		case 4:
			continuationBytes = 3;
			codePoint = firstByte & ~FourByteHeaderMask;
			break;
		default:
			throw InvalidByteSequenceException(0, 0, "Invalid header byte");
		}

		while(continuationBytes--)
		{
			if (!isContinuationByte(*scanPtr))
			{
				throw InvalidByteSequenceException(0, scanPtr - start - 1, "Missing continuation byte");
			}

			codePoint = (codePoint << 6) | (*scanPtr & ~ContinuationHeaderMask);
			scanPtr++;
		}

		const UnicodeChar decodedChar(codePoint);

		if (bytesForChar(decodedChar) != seqBytes)
		{
			// Not a canonical encoding
			throw InvalidByteSequenceException(0, scanPtr - start - 1, "Overlong encoding");
		}

		// Success!
		return scanPtr;
	}
}

std::size_t validateData(const std::uint8_t *start, const std::uint8_t *end)
{
	std::size_t charCount = 0;
	const std::uint8_t *scanPtr = start;

	try
	{
		while(scanPtr != end)
		{
			scanPtr = validateChar(scanPtr, end);
			charCount++;
		}
	}
	catch(const InvalidByteSequenceException &e)
	{
		// Relocate the exception from its character-relative position to an input relative position
		throw e.offsetBy(scanPtr - start);
	}

	return charCount;
}

}
}
