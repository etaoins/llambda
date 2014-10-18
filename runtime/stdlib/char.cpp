#include "binding/CharCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"
#include "binding/RestArgument.h"

#include "unicode/UnicodeChar.h"

using namespace lliby;

namespace
{
	template<class Comparator>
	bool charCompare(UnicodeChar value1, UnicodeChar value2, RestArgument<CharCell> *argHead, Comparator charCompare)
	{
		const ProperList<CharCell> argList(argHead);
		auto argListIt = argList.begin();

		while(true)
		{
			if (!charCompare(value1, value2))
			{
				return false;
			}

			if (argListIt == argList.end())
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

// UnicodeChar is an int32_t. We can't return UnicodeChar here because we have
// C linkage
std::int32_t lliby_integer_to_char(std::int32_t codePoint)
{
	return codePoint;
}

bool lliby_char_equal(UnicodeChar value1, UnicodeChar value2, RestArgument<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 == value2; });
}

bool lliby_char_lt(UnicodeChar value1, UnicodeChar value2, RestArgument<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 < value2; });
}

bool lliby_char_gt(UnicodeChar value1, UnicodeChar value2, RestArgument<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 > value2; });
}

bool lliby_char_lte(UnicodeChar value1, UnicodeChar value2, RestArgument<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 <= value2; });
}

bool lliby_char_gte(UnicodeChar value1, UnicodeChar value2, RestArgument<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 >= value2; });
}

}
