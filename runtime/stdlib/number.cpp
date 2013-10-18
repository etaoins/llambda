#include "binding/BoxedNumeric.h"
#include "binding/BoxedExactInteger.h"
#include "binding/BoxedInexactRational.h"

#include "core/fatal.h"

using namespace lliby;

extern "C"
{

std::int64_t lliby_exact(BoxedNumeric *numeric)
{
	if (auto exactInt = datum_cast<BoxedExactInteger>(numeric))
	{
		// This is already exact
		return exactInt->value();
	}

	// This must be rational; we don't need a type check
	auto inexactRational = static_cast<BoxedInexactRational*>(numeric);

	if (!inexactRational->isInteger())
	{
		_lliby_fatal("Attempted to convert non-integral inexact rational to exact value", numeric);
	}

	return static_cast<std::int64_t>(inexactRational->value());
}

double lliby_inexact(BoxedNumeric *numeric)
{
	if (auto inexactRational = datum_cast<BoxedInexactRational>(numeric))
	{
		// This is already inexact
		return inexactRational->value();
	}

	// This must be an exact int; we don't need a type check
	auto exactInt = static_cast<BoxedExactInteger*>(numeric);

	// Cast to a double
	double inexactValue = static_cast<double>(exactInt->value());

	// Make sure we have the same value now. Integers larger than 2^53 aren't guaranteed to have exact douvble
	// representations
	if (static_cast<std::int64_t>(inexactValue) != exactInt->value())
	{
		_lliby_fatal("Attempted to convert exact integer with a value that cannot be represented by an inexact rational", numeric);
	}

	return inexactValue;
}

}

