#include "UnicodeData.h"
#include <iostream>

namespace lliby
{
namespace UnicodeData
{

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
		};
	};

	struct UnicodeHash
	{
		const AsciiTableEntry *asciiTable;
		const NonAsciiHashBucket *nonAsciiHash;
		unsigned int nonAsciiHashSize;
	};

#include "generated/unicodedatabase.cpp"

	std::int32_t lookupHashedValue(CodePoint codePoint, const UnicodeHash &hash, std::int32_t defaultValue = -1)
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

		unsigned int targetBucket = (std::uint64_t(codePoint) * 2654435761ULL) % hash.nonAsciiHashSize; 
		const NonAsciiHashBucket &bucket = hash.nonAsciiHash[targetBucket];

		if (bucket.chain == nullptr)
		{
			// Nothing here
			return defaultValue;
		}
		else if (bucket.isInline)
		{
			// The value is inline in the hash table
			if (bucket.codePoint == codePoint)
			{
				return bucket.value;
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

	bool codePointInRangeSet(CodePoint codePoint, const UnicodeRangeSet &rangeSet, int min, int max, bool useInitial = false)
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
			// This is an optimization that takes advantage of the fact our tree
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

	bool codePointInRangeSet(CodePoint codePoint, const UnicodeRangeSet &rangeSet)
	{
		return codePointInRangeSet(codePoint, rangeSet, 0, rangeSet.rangeCount - 1, true); 
	}
}

bool isUppercase(CodePoint codePoint)
{
	return codePointInRangeSet(codePoint, UppercaseRangeSet);
}

bool isLowercase(CodePoint codePoint)
{
	return codePointInRangeSet(codePoint, LowercaseRangeSet);
}

bool isAlphabetic(CodePoint codePoint)
{
	return codePointInRangeSet(codePoint, AlphabeticRangeSet);
}

bool isWhitespace(CodePoint codePoint)
{
	return codePointInRangeSet(codePoint, WhitespaceRangeSet);
}
	
bool isNumericDigit(CodePoint codePoint)
{
	return toNumericValue(codePoint) != -1;
}

CodePoint toUppercase(CodePoint codePoint)
{
	return lookupHashedValue(codePoint, ToUpperHash, codePoint);
}

CodePoint toLowercase(CodePoint codePoint)
{
	return lookupHashedValue(codePoint, ToLowerHash, codePoint);
}

NumericValue toNumericValue(CodePoint codePoint)
{
	return lookupHashedValue(codePoint, ToNumericValueHash);
}

CodePoint foldCase(CodePoint codePoint)
{
	return lookupHashedValue(codePoint, ToFoldedHash, codePoint);
}
	
}
}
