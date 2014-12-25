#include "unicode/UnicodeChar.h"

#include "util/charCompare.h"
#include "core/error.h"

using namespace lliby;

extern "C"
{

std::int32_t llbase_char_to_integer(UnicodeChar character)
{
	return character.codePoint();
}

UnicodeChar::CodePoint llbase_integer_to_char(World &world, std::int64_t codePoint)
{
	if ((codePoint < UnicodeChar::FirstCodePoint) || (codePoint > UnicodeChar::LastCodePoint))
	{
		signalError(world, ErrorCategory::Range, "(integer->char) with invalid Unicode code point");
	}

	return codePoint;
}

bool llbase_char_equal(UnicodeChar value1, UnicodeChar value2, RestValues<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 == value2; });
}

bool llbase_char_lt(UnicodeChar value1, UnicodeChar value2, RestValues<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 < value2; });
}

bool llbase_char_gt(UnicodeChar value1, UnicodeChar value2, RestValues<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 > value2; });
}

bool llbase_char_lte(UnicodeChar value1, UnicodeChar value2, RestValues<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 <= value2; });
}

bool llbase_char_gte(UnicodeChar value1, UnicodeChar value2, RestValues<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 >= value2; });
}

}
