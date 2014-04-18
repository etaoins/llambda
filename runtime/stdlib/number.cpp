#include "binding/NumericCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/InexactRationalCell.h"
#include "binding/ProperList.h"

#include <cmath>

#include "core/error.h"

using namespace lliby;

namespace
{
	template<class ExactCompare, class InexactCompare>
	bool numericCompare(World &world, NumericCell *value1, NumericCell *value2, ListElementCell *argHead, ExactCompare exactCompare, InexactCompare inexactCompare)
	{
		const ProperList<NumericCell> argList(argHead);

		auto compareCells = [&] (NumericCell *number1, NumericCell *number2) -> bool
		{
			auto exactNumber1 = datum_cast<ExactIntegerCell>(number1);
			auto exactNumber2 = datum_cast<ExactIntegerCell>(number2);

			if (exactNumber1 && exactNumber2)
			{
				// Both cells are exact
				return exactCompare(exactNumber1->value(), exactNumber2->value());
			}
			else if (!exactNumber1 && !exactNumber2)
			{
				// Both cells are inexact
				auto inexactNumber1 = datum_unchecked_cast<InexactRationalCell>(number1);
				auto inexactNumber2 = datum_unchecked_cast<InexactRationalCell>(number2);

				return inexactCompare(inexactNumber1->value(), inexactNumber2->value());
			}
			else if (!exactNumber1 && exactNumber2)
			{
				auto inexactNumber1 = datum_unchecked_cast<InexactRationalCell>(number1);

				// Try to convert to exact
				auto inexactNumber1AsExact = static_cast<std::int64_t>(inexactNumber1->value());
				if (inexactNumber1->value() == inexactNumber1AsExact)
				{
					// Compare as exact
					return exactCompare(inexactNumber1AsExact, exactNumber2->value());
				}

				// Compare as inexact
				return inexactCompare(inexactNumber1->value(), exactNumber2->value());
			}
			else // if (exactNumber1 && !exactNumber2)
			{
				auto inexactNumber2 = datum_unchecked_cast<InexactRationalCell>(number2);
				
				// Try to convert to exact
				auto inexactNumber2AsExact = static_cast<std::int64_t>(inexactNumber2->value());
				
				if (inexactNumber2->value() == inexactNumber2AsExact)
				{
					// Compare as exact
					return exactCompare(exactNumber1->value(), inexactNumber2AsExact);
				}

				// Compare as inexact
				return inexactCompare(exactNumber1->value(), inexactNumber2->value());
			}
		};

		if (!argList.isValid())
		{
			signalError(world, "Non-numeric passed to comparison function", {argHead});
		}

		if (!compareCells(value1, value2))
		{
			return false;
		}

		NumericCell *prevValue = value2;

		for(auto argListValue : argList)
		{
			if (!compareCells(prevValue, argListValue))
			{
				return false;
			}

			prevValue = argListValue;
		}

		return true;
	}
}

extern "C"
{

std::int64_t lliby_exact(World &world, NumericCell *numeric)
{
	if (auto exactInt = datum_cast<ExactIntegerCell>(numeric))
	{
		// This is already exact
		return exactInt->value();
	}

	// This must be rational; we don't need a type check
	auto inexactRational = datum_unchecked_cast<InexactRationalCell>(numeric);

	if (!inexactRational->isInteger())
	{
		signalError(world, "Attempted to convert non-integral inexact rational to exact value", {numeric});
	}

	return static_cast<std::int64_t>(inexactRational->value());
}

double lliby_inexact(World &world, NumericCell *numeric)
{
	if (auto inexactRational = datum_cast<InexactRationalCell>(numeric))
	{
		// This is already inexact
		return inexactRational->value();
	}

	// This must be an exact int; we don't need a type check
	auto exactInt = datum_unchecked_cast<ExactIntegerCell>(numeric);

	// Cast to a double
	double inexactValue = static_cast<double>(exactInt->value());

	// Make sure we have the same value now. Integers larger than 2^53 aren't guaranteed to have exact douvble
	// representations
	if (static_cast<std::int64_t>(inexactValue) != exactInt->value())
	{
		signalError(world, "Attempted to convert exact integer with a value that cannot be represented by an inexact rational", {numeric});
	}

	return inexactValue;
}

NumericCell *lliby_add(World &world, ListElementCell *argHead)
{
	const ProperList<NumericCell> argList(argHead);

	if (!argList.isValid())
	{
		signalError(world, "Non-numeric passed to (+)", {argHead});
	}

	std::int64_t exactSum = 0;
	double inexactSum = 0.0;
	bool resultInexact = false;

	for (auto numeric : argList)
	{
		if (auto exactInteger = datum_cast<ExactIntegerCell>(numeric))
		{
			exactSum += exactInteger->value();
		}
		else
		{
			auto inexactRational = datum_unchecked_cast<InexactRationalCell>(numeric);

			inexactSum += inexactRational->value();
			resultInexact = true;
		}
	}

	if (resultInexact)
	{
		return InexactRationalCell::fromValue(world, exactSum + inexactSum);
	}
	else
	{
		return ExactIntegerCell::fromValue(world, exactSum);
	}
}

NumericCell *lliby_mul(World &world, ListElementCell *argHead)
{
	const ProperList<NumericCell> argList(argHead);

	if (!argList.isValid())
	{
		signalError(world, "Non-numeric passed to (*)", {argHead});
	}

	std::int64_t exactProduct = 1;
	double inexactProduct = 1.0;
	bool resultInexact = false;

	for (auto numeric : argList)
	{
		if (auto exactInteger = datum_cast<ExactIntegerCell>(numeric))
		{
			exactProduct *= exactInteger->value();
		}
		else
		{
			auto inexactRational = datum_unchecked_cast<InexactRationalCell>(numeric);

			inexactProduct *= inexactRational->value();
			resultInexact = true;
		}
	}

	if (resultInexact)
	{
		return InexactRationalCell::fromValue(world, exactProduct * inexactProduct);
	}
	else
	{
		return ExactIntegerCell::fromValue(world, exactProduct);
	}
}

NumericCell *lliby_sub(World &world, NumericCell *startValue, ListElementCell *argHead)
{
	const ProperList<NumericCell> argList(argHead);

	if (!argList.isValid())
	{
		signalError(world, "Non-numeric passed to (*)", {argHead});
	}

	std::int64_t exactDifference;
	double inexactDifference;
	bool resultInexact;

	if (auto exactInteger = datum_cast<ExactIntegerCell>(startValue))
	{
		if (argList.isEmpty())
		{
			// Return the inverse
			return ExactIntegerCell::fromValue(world, -exactInteger->value());
		}

		exactDifference = exactInteger->value();
		inexactDifference = 0.0;
		resultInexact = false;
	}
	else
	{
		auto inexactRational = datum_unchecked_cast<InexactRationalCell>(startValue);

		if (argList.isEmpty())
		{
			// Return the inverse
			return InexactRationalCell::fromValue(world, -inexactRational->value());
		}

		exactDifference = 0;
		inexactDifference = inexactRational->value();
		resultInexact = true;
	}
	
	for (auto numeric : argList)
	{
		if (auto exactInteger = datum_cast<ExactIntegerCell>(numeric))
		{
			exactDifference -= exactInteger->value();
		}
		else
		{
			auto inexactRational = datum_unchecked_cast<InexactRationalCell>(numeric);

			inexactDifference -= inexactRational->value();
			resultInexact = true;
		}
	}
	
	if (resultInexact)
	{
		return InexactRationalCell::fromValue(world, exactDifference + inexactDifference);
	}
	else
	{
		return ExactIntegerCell::fromValue(world, exactDifference);
	}
}

double lliby_div(World &world, NumericCell *startValue, ListElementCell *argHead)
{
	const ProperList<NumericCell> argList(argHead);

	if (!argList.isValid())
	{
		signalError(world, "Non-numeric passed to (/)", {argHead});
	}

	double currentValue;

	if (auto exactInteger = datum_cast<ExactIntegerCell>(startValue))
	{
		currentValue = exactInteger->value();
	}
	else
	{
		auto inexactRational = datum_unchecked_cast<InexactRationalCell>(startValue);

		currentValue = inexactRational->value();
	}

	if (argList.isEmpty())
	{
		// Return the reciprocal
		return 1.0 / currentValue;
	}
	
	for (auto numeric : argList)
	{
		if (auto exactInteger = datum_cast<ExactIntegerCell>(numeric))
		{
			currentValue /= exactInteger->value();
		}
		else
		{
			auto inexactRational = datum_unchecked_cast<InexactRationalCell>(numeric);

			currentValue /= inexactRational->value();
		}
	}
	
	return currentValue;
}

bool lliby_is_finite(NumericCell *value)
{
	if (auto inexactRational = datum_cast<InexactRationalCell>(value))
	{
		return std::isfinite(inexactRational->value());
	}
	else
	{
		// Exact integers must be finite
		return true;
	}
}

bool lliby_is_infinite(NumericCell *value)
{
	if (auto inexactRational = datum_cast<InexactRationalCell>(value))
	{
		return std::isinf(inexactRational->value());
	}
	else
	{
		// Exact integers cannot be infinite
		return false;
	}
}

bool lliby_is_nan(NumericCell *value)
{
	if (auto inexactRational = datum_cast<InexactRationalCell>(value))
	{
		return std::isnan(inexactRational->value());
	}
	else
	{
		// Exact integers cannot be NaN
		return false;
	}
}

bool lliby_is_zero(NumericCell *value)
{
	if (auto exactInteger = datum_cast<ExactIntegerCell>(value))
	{
		return exactInteger->value() == 0;
	}
	else
	{
		auto inexactRational = datum_unchecked_cast<InexactRationalCell>(value);
		return inexactRational->value() == 0.0;
	}
}

bool lliby_is_odd(std::int64_t value)
{
	// Since C++11 the remainder of a negative number mod a positive is negative
	// Before it was implementation-defined
	return (value % 2) != 0;
}

bool lliby_is_even(std::int64_t value)
{
	return (value % 2) == 0;
}

bool lliby_numeric_equal(World &world, NumericCell *value1, NumericCell *value2, ListElementCell *argHead)
{
	return numericCompare(world, value1, value2, argHead, 
			[] (std::int64_t value1, int64_t value2) { return value1 == value2; },
			[] (double value1, double value2) { return value1 == value2; });
}

bool lliby_numeric_lt(World &world, NumericCell *value1, NumericCell *value2, ListElementCell *argHead)
{
	return numericCompare(world, value1, value2, argHead, 
			[] (std::int64_t value1, int64_t value2) { return value1 < value2; },
			[] (double value1, double value2) { return value1 < value2; });
}

bool lliby_numeric_gt(World &world, NumericCell *value1, NumericCell *value2, ListElementCell *argHead)
{
	return numericCompare(world, value1, value2, argHead, 
			[] (std::int64_t value1, int64_t value2) { return value1 > value2; },
			[] (double value1, double value2) { return value1 > value2; });
}

bool lliby_numeric_lte(World &world, NumericCell *value1, NumericCell *value2, ListElementCell *argHead)
{
	return numericCompare(world, value1, value2, argHead, 
			[] (std::int64_t value1, int64_t value2) { return value1 <= value2; },
			[] (double value1, double value2) { return value1 <= value2; });
}

bool lliby_numeric_gte(World &world, NumericCell *value1, NumericCell *value2, ListElementCell *argHead)
{
	return numericCompare(world, value1, value2, argHead, 
			[] (std::int64_t value1, int64_t value2) { return value1 >= value2; },
			[] (double value1, double value2) { return value1 >= value2; });
}

}
