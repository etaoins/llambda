#ifndef _LLIBY_UTIL_CHARCOMPARE_H
#define _LLIBY_UTIL_CHARCOMPARE_H

#include "binding/ProperList.h"
#include "binding/CharCell.h"

#include "unicode/UnicodeChar.h"

namespace
{
	using namespace lliby;

	template<class Comparator>
	bool charCompare(UnicodeChar value1, UnicodeChar value2, ProperList<CharCell> *argList, Comparator charCompare)
	{
		auto argListIt = argList->begin();

		while(true)
		{
			if (!charCompare(value1, value2))
			{
				return false;
			}

			if (argListIt == argList->end())
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

#endif
