#ifndef _LLIBY_UTIL_STRINGCOMPARE_H
#define _LLIBY_UTIL_STRINGCOMPARE_H

#include "binding/StringCell.h"
#include "binding/ProperList.h"

using namespace lliby;

namespace
{
	template<class Comparator>
	bool stringCompare(StringCell *value1, StringCell *value2, ProperList<StringCell> *argList, Comparator stringCompare)
	{
		auto argListIt = argList->begin();

		while(true)
		{
			if (!stringCompare(value1, value2))
			{
				return false;
			}

			if (argListIt == argList->end())
			{
				// All done!
				return true;
			}

			// Move the the next string pair
			value1 = value2;
			value2 = (*argListIt++);
		}
	}
}

#endif
