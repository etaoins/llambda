#include "binding/AnyCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/StringCell.h"
#include "binding/ProperList.h"

#include "alloc/AbstractRefVector.h"
#include "dynamic/State.h"

#include <cstring>
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

// This isn't in a standard header file
extern char **environ;

void lliby_exit(World &world, AnyCell *exitValue)
{
	dynamic::State::popAllStates(world);
	exit(cellToStatusCode(exitValue));
}

void lliby_emergency_exit(AnyCell *exitValue)
{
	_exit(cellToStatusCode(exitValue));
}

AnyCell *lliby_get_environment_variable(World &world, StringCell *name)
{
	char *value = getenv(name->toUtf8StdString().c_str());

	if (value == nullptr)
	{
		return BooleanCell::falseInstance();
	}
	else
	{
		return StringCell::fromUtf8StdString(world, value);
	}
}

ProperList<ProperList<StringCell>>* lliby_get_environment_variables(World &world)
{
	alloc::StrongRefVector<ProperList<StringCell>> parsedVariables(world);
	alloc::StrongRefVector<StringCell> parsedStrings(world, 2);

	char **scanPtr = environ;

	while(*scanPtr)
	{
		const char *variable = *scanPtr++;

		// Turn in to key-value pair
		const char *separatorPos = std::strchr(variable, '=');

		if (separatorPos == nullptr)
		{
			// No separator
			continue;
		}

		const std::string keyString(variable, separatorPos);
		const std::string valueString(separatorPos + 1);

		parsedStrings[0] = StringCell::fromUtf8StdString(world, keyString);
		parsedStrings[1] = StringCell::fromUtf8StdString(world, valueString);

		parsedVariables.push_back(ProperList<StringCell>::create(world, parsedStrings));
	}

	return ProperList<ProperList<StringCell>>::create(world, parsedVariables);
}

}
