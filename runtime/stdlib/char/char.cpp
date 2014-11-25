#include "binding/CharCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"

#include "unicode/UnicodeChar.h"
#include "ucd/ucd.h"

#include "util/stringCompare.h"
#include "util/charCompare.h"

using namespace lliby;

extern "C"
{

bool llchar_char_is_alphabetic(UnicodeChar character)
{
	return ucd::isAlphabetic(character);
}

bool llchar_char_is_numeric(UnicodeChar character)
{
	return ucd::isNumericDigit(character);
}

bool llchar_char_is_whitespace(UnicodeChar character)
{
	return ucd::isWhitespace(character);
}

bool llchar_char_is_upper_case(UnicodeChar character)
{
	return ucd::isUppercase(character);
}

bool llchar_char_is_lower_case(UnicodeChar character)
{
	return ucd::isLowercase(character);
}

// UnicodeChar is an int32_t. We can't return UnicodeChar here because we have C linkage
std::int32_t llchar_char_upcase(UnicodeChar character)
{
	return ucd::toUppercase(character).codePoint();
}

std::int32_t llchar_char_downcase(UnicodeChar character)
{
	return ucd::toLowercase(character).codePoint();
}

std::int32_t llchar_char_foldcase(UnicodeChar character)
{
	return ucd::toCaseFolded(character).codePoint();
}

const AnyCell *llchar_digit_value(World &world, UnicodeChar character)
{
	ucd::DigitValue digitValue = ucd::digitValue(character);

	if (digitValue == ucd::InvalidDigitValue)
	{
		// Not a digit. Return #f
		return BooleanCell::falseInstance();
	}

	return ExactIntegerCell::fromValue(world, digitValue);
}

StringCell* llchar_string_upcase(World &world, StringCell *sourceString)
{
	return sourceString->toConvertedString(world, ucd::toUppercase);
}

StringCell* llchar_string_downcase(World &world, StringCell *sourceString)
{
	return sourceString->toConvertedString(world, ucd::toLowercase);
}

StringCell* llchar_string_foldcase(World &world, StringCell *sourceString)
{
	return sourceString->toConvertedString(world, ucd::toCaseFolded);
}

bool llchar_string_ci_equal(StringCell *value1, StringCell *value2, ProperList<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return value1->compare(value2, ucd::toCaseFolded) == 0; });
}

bool llchar_string_ci_lt(StringCell *value1, StringCell *value2, ProperList<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return value1->compare(value2, ucd::toCaseFolded) < 0; });
}

bool llchar_string_ci_gt(StringCell *value1, StringCell *value2, ProperList<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return value1->compare(value2, ucd::toCaseFolded) > 0; });
}

bool llchar_string_ci_lte(StringCell *value1, StringCell *value2, ProperList<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return value1->compare(value2, ucd::toCaseFolded) <= 0; });
}

bool llchar_string_ci_gte(StringCell *value1, StringCell *value2, ProperList<StringCell> *argHead)
{
	return stringCompare(value1, value2, argHead,
			[] (StringCell *value1, StringCell *value2) { return value1->compare(value2, ucd::toCaseFolded) >= 0; });
}

}
