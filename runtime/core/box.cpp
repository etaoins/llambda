#include "unicode/UnicodeChar.h"

#include "binding/ExactIntegerCell.h"
#include "binding/InexactRationalCell.h"
#include "binding/CharacterCell.h"
#include "binding/StringCell.h"

using namespace lliby;

extern "C"
{

StringCell *_lliby_string_from_utf8(const char *str)
{
	return StringCell::fromUtf8CString(str);
}

}
