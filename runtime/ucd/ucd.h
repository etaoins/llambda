#ifndef _LLIBY_UCD_UCD_H
#define _LLIBY_UCD_UCD_H

#include "unicode/UnicodeChar.h"

namespace lliby
{
/**
 * Contains functions related the the Unicode character database
 */
namespace ucd
{
	bool isUppercase(UnicodeChar c);
	bool isLowercase(UnicodeChar c);
	bool isAlphabetic(UnicodeChar c);
	bool isWhitespace(UnicodeChar c);
	bool isNumericDigit(UnicodeChar c);

	UnicodeChar toUppercase(UnicodeChar c);
	UnicodeChar toLowercase(UnicodeChar c);
	UnicodeChar toCaseFolded(UnicodeChar c);

	typedef std::int32_t DigitValue;
	static const DigitValue InvalidDigitValue = -1;

	DigitValue digitValue(UnicodeChar c);
}
}

#endif
