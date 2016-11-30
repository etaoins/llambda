#include "binding/NumberCell.h"
#include "binding/IntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/ProperList.h"

#include <cmath>

#include "core/error.h"

using namespace lliby;

namespace
{
	template<class IntegerCompare, class FlonumCompare>
	bool numericCompare(NumberCell *value1, NumberCell *value2, RestValues<NumberCell> *argHead, IntegerCompare integerCompare, FlonumCompare flonumCompare)
	{
		auto compareCells = [&] (NumberCell *number1, NumberCell *number2) -> bool
		{
			auto integerNumber1 = cell_cast<IntegerCell>(number1);
			auto integerNumber2 = cell_cast<IntegerCell>(number2);

			if (integerNumber1 && integerNumber2)
			{
				// Both cells are integers
				return integerCompare(integerNumber1->value(), integerNumber2->value());
			}
			else if (!integerNumber1 && !integerNumber2)
			{
				// Both cells are flonums
				auto flonumNumber1 = cell_unchecked_cast<FlonumCell>(number1);
				auto flonumNumber2 = cell_unchecked_cast<FlonumCell>(number2);

				return flonumCompare(flonumNumber1->value(), flonumNumber2->value());
			}
			else if (!integerNumber1 && integerNumber2)
			{
				auto flonumNumber1 = cell_unchecked_cast<FlonumCell>(number1);

				// Try to convert to integer
				auto flonumNumber1AsInteger = static_cast<std::int64_t>(flonumNumber1->value());
				if (flonumNumber1->value() == flonumNumber1AsInteger)
				{
					// Compare as integer
					return integerCompare(flonumNumber1AsInteger, integerNumber2->value());
				}

				// Compare as inteer
				return flonumCompare(flonumNumber1->value(), integerNumber2->value());
			}
			else // if (integerNumber1 && !integerNumber2)
			{
				auto flonumNumber2 = cell_unchecked_cast<FlonumCell>(number2);

				// Try to convert to integer
				auto flonumNumber2AsInteger = static_cast<std::int64_t>(flonumNumber2->value());

				if (flonumNumber2->value() == flonumNumber2AsInteger)
				{
					// Compare as integer
					return integerCompare(integerNumber1->value(), flonumNumber2AsInteger);
				}

				// Compare as flonum
				return flonumCompare(integerNumber1->value(), flonumNumber2->value());
			}
		};

		if (!compareCells(value1, value2))
		{
			return false;
		}

		NumberCell *prevValue = value2;

		for(auto argListValue : *argHead)
		{
			if (!compareCells(prevValue, argListValue))
			{
				return false;
			}

			prevValue = argListValue;
		}

		return true;
	}

	template<class IntegerCompare, class FlonumCompare>
	NumberCell *selectNumericValue(World &world, NumberCell *initialNumber, RestValues<NumberCell> *argHead, IntegerCompare integerCompare, FlonumCompare flonumCompare)
	{
		NumberCell *selectedNumber = initialNumber;
		bool resultIsFlonum = true;

		for(auto otherNumber : *argHead)
		{
			auto selectedInt = cell_cast<IntegerCell>(selectedNumber);
			auto otherInt = cell_cast<IntegerCell>(otherNumber);

			if (selectedInt && otherInt)
			{
				// We can compare these as integers
				if (integerCompare(otherInt->value(), selectedInt->value()))
				{
					// Switch the selected number
					selectedNumber = otherInt;
				}
			}
			else
			{
				// We have to compare these as doubles - even if we don't select the flonum value the entire result
				// becomes flonum
				resultIsFlonum = false;

				if (flonumCompare(otherNumber->toDouble(), selectedNumber->toDouble()))
				{
					selectedNumber = otherNumber;
				}
			}
		}

		if (!resultIsFlonum && IntegerCell::isInstance(selectedNumber))
		{
			// We have to allocate a new cell to convert this number to flonum
			return FlonumCell::fromValue(world, selectedNumber->toDouble());
		}
		else
		{
			return selectedNumber;
		}
	}
}

extern "C"
{

std::int64_t llbase_integer(World &world, NumberCell *numeric)
{
	if (auto integer = cell_cast<IntegerCell>(numeric))
	{
		// This is already an integer
		return integer->value();
	}

	// This must be a flonum; we don't need a type check
	auto flonum = cell_unchecked_cast<FlonumCell>(numeric);

	if (!flonum->isIntegral())
	{
		signalError(world, ErrorCategory::InvalidArgument, "Attempted to convert non-integral flonum to integer", {numeric});
	}

	return static_cast<std::int64_t>(flonum->value());
}

double llbase_flonum(NumberCell *numeric)
{
	if (auto flonum = cell_cast<FlonumCell>(numeric))
	{
		// This is already a flonum
		return flonum->value();
	}

	// This must be an integer; we don't need a type check
	auto integer = cell_unchecked_cast<IntegerCell>(numeric);

	// Cast to a double
	return static_cast<double>(integer->value());
}

bool llbase_is_rational(AnyCell *anyCell)
{
	if (IntegerCell::isInstance(anyCell))
	{
		return true;
	}
	else if (auto flonumCell = cell_cast<FlonumCell>(anyCell))
	{
		double doubleValue = flonumCell->value();
		return !std::isnan(doubleValue) && !std::isinf(doubleValue);
	}
	else
	{
		// Not a number
		return false;
	}
}

bool llbase_numeric_equal(NumberCell *value1, NumberCell *value2, RestValues<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 == value2; },
			[] (double value1, double value2) { return value1 == value2; });
}

bool llbase_numeric_lt(NumberCell *value1, NumberCell *value2, RestValues<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 < value2; },
			[] (double value1, double value2) { return value1 < value2; });
}

bool llbase_numeric_gt(NumberCell *value1, NumberCell *value2, RestValues<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 > value2; },
			[] (double value1, double value2) { return value1 > value2; });
}

bool llbase_numeric_lte(NumberCell *value1, NumberCell *value2, RestValues<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 <= value2; },
			[] (double value1, double value2) { return value1 <= value2; });
}

bool llbase_numeric_gte(NumberCell *value1, NumberCell *value2, RestValues<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 >= value2; },
			[] (double value1, double value2) { return value1 >= value2; });
}

NumberCell *llbase_max(World &world, NumberCell *currentMaxNumber, RestValues<NumberCell> *argHead)
{
	return selectNumericValue(world, currentMaxNumber, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 > value2; },
			[] (double value1, double value2) { return value1 > value2; });
}

NumberCell *llbase_min(World &world, NumberCell *currentMaxNumber, RestValues<NumberCell> *argHead)
{
	return selectNumericValue(world, currentMaxNumber, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 < value2; },
			[] (double value1, double value2) { return value1 < value2; });
}

}
