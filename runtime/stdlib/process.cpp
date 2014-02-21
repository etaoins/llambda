#include "binding/DatumCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/InexactRationalCell.h"

#include "dynamic/State.h"

#include <stdlib.h>
#include <unistd.h>

using namespace lliby;

namespace
{
	int datumToStatusCode(DatumCell *datum)
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
		else if (auto exactInt = datum_cast<ExactIntegerCell>(datum))
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

void lliby_exit(World &world, DatumCell *exitValue)
{
	dynamic::State::popAllStates(world);
	exit(datumToStatusCode(exitValue));
}

void lliby_emergency_exit(DatumCell *exitValue)
{
	_exit(datumToStatusCode(exitValue));
}

}
