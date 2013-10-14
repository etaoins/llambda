#include "binding/BoxedCharacter.h"
#include "binding/BoxedExactInteger.h"
#include "binding/BoxedBoolean.h"
#include "unicode/UnicodeChar.h"

using namespace lliby;

extern "C"
{

const BoxedDatum *lliby_digit_value(UnicodeChar character)
{
	UnicodeChar::DigitValue digitValue = character.digitValue();

	if (digitValue == UnicodeChar::InvalidDigitValue)
	{
		// Not a digit. Return #f
		return BoxedBoolean::falseInstance();
	}

	return new BoxedExactInteger(digitValue);
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
