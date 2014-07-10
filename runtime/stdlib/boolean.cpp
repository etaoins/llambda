#include "binding/ListElementCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"

#include "core/error.h"

extern "C"
{

using namespace lliby;

bool lliby_boolean_equal(World &world, BooleanCell *value1, BooleanCell *value2, ListElementCell *argHead)
{
	if (value1 != value2)
	{
		return false;
	}
	
	ProperList<BooleanCell> properList(argHead);

	if (!properList.isValid())
	{
		signalError(world, "Non-boolean passed to (boolean=?)", {argHead});
	}

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
