#include "binding/AnyCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"

#include "dynamic/State.h"

#include <stdlib.h>
#include <unistd.h>

using namespace lliby;

namespace
{
	int cellToStatusCode(AnyCell *datum)
	{
		if (datum == BooleanCell::trueInstance())
		{
			// #t is success
			return 0;
		}
		else if (datum == BooleanCell::falseInstance())
		{
			// #f is failure
			return -1;
		}
		else if (auto exactInt = cell_cast<ExactIntegerCell>(datum))
		{
			// Return the exact integer
			return exactInt->value();
		}

		// XXX: Should we warn here?
		return 0;
	}
}

extern "C"
{

void lliby_exit(World &world, AnyCell *exitValue)
{
	dynamic::State::popAllStates(world);
	exit(cellToStatusCode(exitValue));
}

void lliby_emergency_exit(AnyCell *exitValue)
{
	_exit(cellToStatusCode(exitValue));
}

}
