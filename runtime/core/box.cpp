#include "unicode/UnicodeChar.h"

#include "binding/BoxedExactInteger.h"
#include "binding/BoxedInexactRational.h"
#include "binding/BoxedCharacter.h"
#include "binding/BoxedString.h"

using namespace lliby;

extern "C"
{

BoxedString *_lliby_string_from_utf8(const char *str)
{
	return BoxedString::fromUtf8CString(str);
}

}
