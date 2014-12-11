#include "unicode/UnicodeChar.h"

#include "util/charCompare.h"

using namespace lliby;

extern "C"
{

std::int32_t llbase_char_to_integer(UnicodeChar character)
{
	return character.codePoint();
}

std::int32_t llbase_integer_to_char(std::int32_t codePoint)
{
	return codePoint;
}

bool llbase_char_equal(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 == value2; });
}

bool llbase_char_lt(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 < value2; });
}

bool llbase_char_gt(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 > value2; });
}

bool llbase_char_lte(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 <= value2; });
}

bool llbase_char_gte(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argHead)
{
	return charCompare(value1, value2, argHead,
			[] (UnicodeChar value1, UnicodeChar value2) { return value1 >= value2; });
}

}
