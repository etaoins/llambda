#include "ucd/ucd.h"

namespace
{
	struct UnicodeRange
	{
		std::uint32_t startCodePoint;
		std::uint32_t endCodePoint;
	};

	struct UnicodeRangeSet
	{
		const UnicodeRange *ranges;
		int rangeCount;
		int entryRange;
	};

	// This assumes that we only map ASCII characters to other ASCII characters
	// This seems reasonable and if it's violated -Werror will fail our compile
	// due to integer truncation
	typedef std::int8_t AsciiTableEntry;

	struct NonAsciiHashChain
	{
		std::uint32_t isLast : 1;
		std::uint32_t codePoint : 31;
		std::uint32_t value;
	};

	union NonAsciiHashBucket
	{
		const NonAsciiHashChain *chain;

		struct
		{
			std::uint32_t codePoint;
			std::uint32_t value : 31;
			std::uint32_t isInline : 1;
		} inlineBucket;
	};

	struct UnicodeHash
	{
		const AsciiTableEntry *asciiTable;
		const NonAsciiHashBucket *nonAsciiHash;
		unsigned int nonAsciiHashSize;
	};

#include "generated/unicodedatabase.cpp"

	std::int32_t lookupHashedValue(std::int32_t codePoint, const UnicodeHash &hash, std::int32_t defaultValue = -1)
	{
		if (codePoint <= 127)
		{
			// Easy
			const std::int32_t value = hash.asciiTable[codePoint];

			if (value == -1)
			{
				return defaultValue;
			}

			return value;
		}

		// Note this unsigned overflow in both intentional and defined in C++
		unsigned int targetBucket = std::uint32_t(codePoint * 2654435761) % hash.nonAsciiHashSize; 
		const NonAsciiHashBucket &bucket = hash.nonAsciiHash[targetBucket];

		if (bucket.chain == nullptr)
		{
			// Nothing here
			return defaultValue;
		}
		else if (bucket.inlineBucket.isInline)
		{
			// The value is inline in the hash table
			if (bucket.inlineBucket.codePoint == codePoint)
			{
				return bucket.inlineBucket.value;
			}
			
			return defaultValue;
		}

		const NonAsciiHashChain *chain = bucket.chain;

		while(true)
		{
			if (chain->codePoint == codePoint)
			{
				return chain->value;
			}
			else if (chain->isLast)
			{
				return defaultValue;
			}

			chain++;
		}
	}

	bool codePointInRangeSet(std::int32_t codePoint, const UnicodeRangeSet &rangeSet, int min, int max, bool useInitial = false)
	{
		if (min > max)
		{
			// Couldn't find it
			return false;
		}

		unsigned int testIndex;

		if (useInitial)
		{
			// The Python database generator picks an entry point biased towards 
			// ASCII values
			testIndex = rangeSet.entryRange;
		}
		else
		{
			testIndex = min + ((max - min) / 2);
		}

		const UnicodeRange &testRange = rangeSet.ranges[testIndex];
		
		if (codePoint < testRange.startCodePoint)
		{
			// We're on the left hand side
			return codePointInRangeSet(codePoint, rangeSet, min, testIndex - 1);
		}
		else if (codePoint > testRange.endCodePoint)
		{
			// Test the very next range to make sure we're not in the gap
			// between ranges.
			//
			// This is an optimisation that takes advantage of the fact our tree
			// is biased towards ASCII values and that there's usually a large
			// gap after the ASCII ranges before their Unicode counterparts  
			// start. With this most ASCII queries finish after 1-2 iterations.
			// 
			// This should be mostly harmless for non-ASCII values as the next
			// range has a high likelihood of being in cache
			if (testIndex < (rangeSet.rangeCount - 1))
			{
				const UnicodeRange &nextRange = rangeSet.ranges[testIndex + 1];

				if (codePoint < nextRange.startCodePoint)
				{
					// We're in the gap between ranges
					return false;
				}
			}

			// We're on the righthand side
			return codePointInRangeSet(codePoint, rangeSet, testIndex + 1, max);
		}
		else
		{
			// Yay!
			return true;
		}
	}

	bool codePointInRangeSet(std::int32_t codePoint, const UnicodeRangeSet &rangeSet)
	{
		return codePointInRangeSet(codePoint, rangeSet, 0, rangeSet.rangeCount - 1, true); 
	}
}

namespace lliby
{
namespace ucd
{

bool isUppercase(UnicodeChar c)
{
	return codePointInRangeSet(c.codePoint(), UppercaseRangeSet);
}

bool isLowercase(UnicodeChar c)
{
	return codePointInRangeSet(c.codePoint(), LowercaseRangeSet);
}

bool isAlphabetic(UnicodeChar c)
{
	return codePointInRangeSet(c.codePoint(), AlphabeticRangeSet);
}

bool isWhitespace(UnicodeChar c)
{
	return codePointInRangeSet(c.codePoint(), WhitespaceRangeSet);
}

bool isNumericDigit(UnicodeChar c)
{
	return digitValue(c) != InvalidDigitValue;
}

UnicodeChar toUppercase(UnicodeChar c)
{
	return UnicodeChar(lookupHashedValue(c.codePoint(), ToUpperHash, c.codePoint()));
}

UnicodeChar toLowercase(UnicodeChar c)
{
	return UnicodeChar(lookupHashedValue(c.codePoint(), ToLowerHash, c.codePoint()));
}

UnicodeChar toCaseFolded(UnicodeChar c)
{
	return UnicodeChar(lookupHashedValue(c.codePoint(), ToFoldedHash, c.codePoint()));
}

DigitValue digitValue(UnicodeChar c)
{
	return lookupHashedValue(c.codePoint(), ToNumericValueHash);
}

}
}
