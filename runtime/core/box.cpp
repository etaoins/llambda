#include "unicode/UnicodeChar.h"

#include "binding/BoxedExactInteger.h"
#include "binding/BoxedInexactRational.h"
#include "binding/BoxedCharacter.h"
#include "binding/BoxedString.h"

using namespace lliby;

extern "C"
{

BoxedExactInteger *_lliby_box_exact_integer(std::int64_t value)
{
	return new BoxedExactInteger(value);
}

BoxedInexactRational *_lliby_box_inexact_rational(double value)
{
	return new BoxedInexactRational(value);
}

BoxedCharacter *_lliby_box_character(UnicodeChar value)
{
	return new BoxedCharacter(value);
}

BoxedString *_lliby_string_from_utf8(const char *str)
{
	return BoxedString::fromUtf8CString(str);
}

}
