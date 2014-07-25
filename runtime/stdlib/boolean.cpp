#include "binding/ListElementCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"
#include "binding/RestArgument.h"

#include "core/error.h"

extern "C"
{

using namespace lliby;

bool lliby_boolean_equal(BooleanCell *value1, BooleanCell *value2, RestArgument<BooleanCell> *argHead)
{
	if (value1 != value2)
	{
		return false;
	}
	
	ProperList<BooleanCell> properList(argHead);

	for(auto boolCell : properList)
	{
		if (boolCell != value1)
		{
			return false;
		}
	}

	return true;
}

}
