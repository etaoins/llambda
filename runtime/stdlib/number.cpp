#include "binding/BoxedNumeric.h"
#include "binding/BoxedExactInteger.h"
#include "binding/BoxedInexactRational.h"
#include "binding/ProperList.h"

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

BoxedNumeric *lliby_add(BoxedListElement *argHead)
{
	const ProperList<BoxedNumeric> argList(argHead);

	if (!argList.isValid())
	{
		_lliby_fatal("Non-numeric passed to (+)", argHead);
	}

	std::int64_t exactSum = 0;
	double inexactSum = 0.0;
	bool resultInexact = false;

	for (auto numeric : argList)
	{
		if (auto exactInteger = datum_cast<BoxedExactInteger>(numeric))
		{
			exactSum += exactInteger->value();
		}
		else
		{
			auto inexactRational = static_cast<BoxedInexactRational*>(numeric);

			inexactSum += inexactRational->value();
			resultInexact = true;
		}
	}

	if (resultInexact)
	{
		return new BoxedInexactRational(exactSum + inexactSum);
	}
	else
	{
		return new BoxedExactInteger(exactSum);
	}
}

BoxedNumeric *lliby_mul(BoxedListElement *argHead)
{
	const ProperList<BoxedNumeric> argList(argHead);

	if (!argList.isValid())
	{
		_lliby_fatal("Non-numeric passed to (*)", argHead);
	}

	std::int64_t exactProduct = 1;
	double inexactProduct = 1.0;
	bool resultInexact = false;

	for (auto numeric : argList)
	{
		if (auto exactInteger = datum_cast<BoxedExactInteger>(numeric))
		{
			exactProduct *= exactInteger->value();
		}
		else
		{
			auto inexactRational = static_cast<BoxedInexactRational*>(numeric);

			inexactProduct *= inexactRational->value();
			resultInexact = true;
		}
	}

	if (resultInexact)
	{
		return new BoxedInexactRational(exactProduct * inexactProduct);
	}
	else
	{
		return new BoxedExactInteger(exactProduct);
	}
}

BoxedNumeric *lliby_sub(BoxedNumeric *startValue, BoxedListElement *argHead)
{
	const ProperList<BoxedNumeric> argList(argHead);

	if (!argList.isValid())
	{
		_lliby_fatal("Non-numeric passed to (*)", argHead);
	}

	std::int64_t exactDifference;
	double inexactDifference;
	bool resultInexact;

	if (auto exactInteger = datum_cast<BoxedExactInteger>(startValue))
	{
		if (argList.isEmpty())
		{
			// Return the inverse
			return new BoxedExactInteger(-exactInteger->value());
		}

		exactDifference = exactInteger->value();
		inexactDifference = 0.0;
		resultInexact = false;
	}
	else
	{
		auto inexactRational = static_cast<BoxedInexactRational*>(startValue);

		if (argList.isEmpty())
		{
			// Return the inverse
			return new BoxedInexactRational(-inexactRational->value());
		}

		exactDifference = 0;
		inexactDifference = inexactRational->value();
		resultInexact = true;
	}
	
	for (auto numeric : argList)
	{
		if (auto exactInteger = datum_cast<BoxedExactInteger>(numeric))
		{
			exactDifference -= exactInteger->value();
		}
		else
		{
			auto inexactRational = static_cast<BoxedInexactRational*>(numeric);

			inexactDifference -= inexactRational->value();
			resultInexact = true;
		}
	}
	
	if (resultInexact)
	{
		return new BoxedInexactRational(exactDifference + inexactDifference);
	}
	else
	{
		return new BoxedExactInteger(exactDifference);
	}
}

double lliby_div(BoxedNumeric *startValue, BoxedListElement *argHead)
{
	const ProperList<BoxedNumeric> argList(argHead);

	if (!argList.isValid())
	{
		_lliby_fatal("Non-numeric passed to (/)", argHead);
	}

	double currentValue;

	if (auto exactInteger = datum_cast<BoxedExactInteger>(startValue))
	{
		currentValue = exactInteger->value();
	}
	else
	{
		auto inexactRational = static_cast<BoxedInexactRational*>(startValue);

		currentValue = inexactRational->value();
	}

	if (argList.isEmpty())
	{
		// Return the reciprocal
		return 1.0 / currentValue;
	}
	
	for (auto numeric : argList)
	{
		if (auto exactInteger = datum_cast<BoxedExactInteger>(numeric))
		{
			currentValue /= exactInteger->value();
		}
		else
		{
			auto inexactRational = static_cast<BoxedInexactRational*>(numeric);

			currentValue /= inexactRational->value();
		}
	}
	
	return currentValue;
}

}
