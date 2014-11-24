#include "utf8.h"

#include "utf8/InvalidByteSequenceException.h"

namespace lliby
{
namespace utf8
{


std::size_t validateData(const std::uint8_t *start, const std::uint8_t *end)
{
	std::size_t charCount = 0;
	const std::uint8_t *scanPtr = start;

	while(scanPtr != end)
	{
		const std::size_t charByteOffset = scanPtr - start;
		const std::size_t inputBytes = end - scanPtr;

		const std::uint8_t firstByte = *(scanPtr++);
		const int seqBytes = bytesInSequence(firstByte);

		unsigned int continuationBytes;

		if (seqBytes < 1)
		{
			// Invalid header byte
			throw InvalidHeaderByteException(charCount, charByteOffset);
		}

		if (inputBytes < seqBytes)
		{
			// Not enough input
			throw TruncatedInputException(charCount, charByteOffset, end - start - 1);
		}

		std::int32_t codePoint;

		switch(seqBytes)
		{
		case 1:
			// ASCII - nothing to do
			charCount++;
			continue;
		case 2:
			continuationBytes = 1;
			codePoint = firstByte & ~TwoByteHeaderValue;
			break;
		case 3:
			continuationBytes = 2;
			codePoint = firstByte & ~ThreeByteHeaderValue;
			break;
		case 4:
			continuationBytes = 3;
			codePoint = firstByte & ~FourByteHeaderValue;
			break;
		default:
			throw InvalidHeaderByteException(charCount, charByteOffset);
		}

		while(continuationBytes--)
		{
			if (!isContinuationByte(*scanPtr))
			{
				throw MissingContinuationByteException(charCount, charByteOffset, scanPtr - start - 1);
			}

			codePoint = (codePoint << 6) | (*scanPtr & ~ContinuationHeaderMask);
			scanPtr++;
		}

		const UnicodeChar decodedChar(codePoint);

		if (bytesForChar(decodedChar) != seqBytes)
		{
			// Not a canonical encoding
			throw OverlongEncodingException(charCount, charByteOffset, scanPtr - start - 1);
		}

		// Success!
		charCount++;
	}

	return charCount;
}

}
}
