#include "binding/BoxedListElement.h"
#include "binding/BoxedBoolean.h"
#include "binding/ProperList.h"
#include "core/fatal.h"

extern "C"
{

using namespace lliby;

bool lliby_not(bool value)
{
	return !value;
}

bool lliby_boolean_equal(bool value1, bool value2, BoxedListElement *argHead)
{
	if (value1 != value2)
	{
		return false;
	}
	
	ProperList<BoxedBoolean> properList(argHead);

	if (!properList.isValid())
	{
		// We're not supposed to abort here, just return false
		_lliby_fatal("Non-boolean passed to (boolean=?)", argHead);
		return false;
	}

	for(auto boxedBool : properList)
	{
		if (boxedBool->value() != value1)
		{
			return false;
		}
	}

	return true;
}

}
