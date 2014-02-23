#include "binding/CharacterCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/BooleanCell.h"
#include "unicode/UnicodeChar.h"

using namespace lliby;

extern "C"
{

const DatumCell *lliby_digit_value(World &world, UnicodeChar character)
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

}
