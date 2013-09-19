#ifndef _LLIBY_UNICODEDATA_H
#define _LLIBY_UNICODEDATA_H

#include <cstdint>

namespace lliby
{

namespace UnicodeData
{
	typedef std::int32_t CodePoint;

	typedef std::int32_t NumericValue;
	static const NumericValue InvalidNumericValue = -1;

	bool isUppercase(CodePoint);
	bool isLowercase(CodePoint);
	bool isAlphabetic(CodePoint);
	bool isWhitespace(CodePoint);
	bool isNumericDigit(CodePoint);

	CodePoint toUppercase(CodePoint);
	CodePoint toLowercase(CodePoint);
	NumericValue toNumericValue(CodePoint);
	
	CodePoint foldCase(CodePoint);
}

};

#endif

