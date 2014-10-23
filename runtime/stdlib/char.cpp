#include "binding/CharCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"

#include "unicode/UnicodeChar.h"

using namespace lliby;

namespace
{
	template<class Comparator>
	bool charCompare(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argList, Comparator charCompare)
	{
		auto argListIt = argList->begin();

		while(true)
		{
			if (!charCompare(value1, value2))
			{
				return false;
			}

			if (argListIt == argList->end())
			{
				// All done!
				return true;
			}

			// Move the the next character pair
			value1 = value2;
			value2 = (*argListIt++)->unicodeChar();
		}
	}
}

extern "C"
{

bool lliby_char_is_alphabetic(UnicodeChar character)
{
	return character.isAlphabetic();
}

bool lliby_char_is_numeric(UnicodeChar character)
{
	return character.isNumericDigit();
}

bool lliby_char_is_whitespace(UnicodeChar character)
{
	return character.isWhitespace();
}

bool lliby_char_is_upper_case(UnicodeChar character)
{
	return character.isUppercase();
}

bool lliby_char_is_lower_case(UnicodeChar character)
{
	return character.isLowercase();
}

// UnicodeChar is an int32_t. We can't return UnicodeChar here because we have C linkage
std::int32_t lliby_char_upcase(UnicodeChar character)
{
	return character.toUppercase().codePoint();
}

std::int32_t lliby_char_downcase(UnicodeChar character)
{
	return character.toLowercase().codePoint();
}

std::int32_t lliby_char_foldcase(UnicodeChar character)
{
	return character.toCaseFolded().codePoint();
}

const AnyCell *lliby_digit_value(World &world, UnicodeChar character)
{
	UnicodeChar::DigitValue digitValue = character.digitValue();

	if (digitValue == UnicodeChar::InvalidDigitValue)
	{
		// Not a digit. Return #f
		return BooleanCell::falseInstance();
	}

	return ExactIntegerCell::fromValue(world, digitValue);
}

std::int32_t lliby_char_to_integer(UnicodeChar character)
{
	return character.codePoint();
}

std::int32_t lliby_integer_to_char(std::int32_t codePoint)
{
	return codePoint;
}

bool lliby_char_equal(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 == value2; });
}

bool lliby_char_lt(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 < value2; });
}

bool lliby_char_gt(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 > value2; });
}

bool lliby_char_lte(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 <= value2; });
}

bool lliby_char_gte(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 >= value2; });
}

}
