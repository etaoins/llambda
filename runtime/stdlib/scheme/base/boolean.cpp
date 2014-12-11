#include "binding/BooleanCell.h"
#include "binding/ProperList.h"

#include "core/error.h"

extern "C"
{

using namespace lliby;

bool llbase_boolean_equal(BooleanCell *value1, BooleanCell *value2, ProperList<BooleanCell> *argList)
{
	if (value1 != value2)
	{
		return false;
	}

	for(auto boolCell : *argList)
	{
		if (boolCell != value1)
		{
			return false;
		}
	}

	return true;
}

}
