#include "binding/NumberCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/ProperList.h"

#include <cmath>

#include "core/error.h"

using namespace lliby;

namespace
{
	template<class ExactCompare, class InexactCompare>
	bool numericCompare(NumberCell *value1, NumberCell *value2, ProperList<NumberCell> *argHead, ExactCompare exactCompare, InexactCompare inexactCompare)
	{
		auto compareCells = [&] (NumberCell *number1, NumberCell *number2) -> bool
		{
			auto exactNumber1 = cell_cast<ExactIntegerCell>(number1);
			auto exactNumber2 = cell_cast<ExactIntegerCell>(number2);

			if (exactNumber1 && exactNumber2)
			{
				// Both cells are exact
				return exactCompare(exactNumber1->value(), exactNumber2->value());
			}
			else if (!exactNumber1 && !exactNumber2)
			{
				// Both cells are inexact
				auto inexactNumber1 = cell_unchecked_cast<FlonumCell>(number1);
				auto inexactNumber2 = cell_unchecked_cast<FlonumCell>(number2);

				return inexactCompare(inexactNumber1->value(), inexactNumber2->value());
			}
			else if (!exactNumber1 && exactNumber2)
			{
				auto inexactNumber1 = cell_unchecked_cast<FlonumCell>(number1);

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
				auto inexactNumber2 = cell_unchecked_cast<FlonumCell>(number2);

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

	template<class ExactCompare, class InexactCompare>
	NumberCell *selectNumericValue(World &world, NumberCell *initialNumber, ProperList<NumberCell> *argHead, ExactCompare exactCompare, InexactCompare inexactCompare)
	{
		NumberCell *selectedNumber = initialNumber;
		bool resultExact = true;

		for(auto otherNumber : *argHead)
		{
			auto selectedInt = cell_cast<ExactIntegerCell>(selectedNumber);
			auto otherInt = cell_cast<ExactIntegerCell>(otherNumber);

			if (selectedInt && otherInt)
			{
				// We can compare these as integers
				if (exactCompare(otherInt->value(), selectedInt->value()))
				{
					// Switch the selected number
					selectedNumber = otherInt;
				}
			}
			else
			{
				// We have to compare these as doubles - even if we don't select the inexact value the entire result
				// becomes inexact
				resultExact = false;

				if (inexactCompare(otherNumber->toDouble(), selectedNumber->toDouble()))
				{
					selectedNumber = otherNumber;
				}
			}
		}

		if (!resultExact && selectedNumber->isExact())
		{
			// We have to allocate a new cell to de-exact this number
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

std::int64_t lliby_exact(World &world, NumberCell *numeric)
{
	if (auto exactInt = cell_cast<ExactIntegerCell>(numeric))
	{
		// This is already exact
		return exactInt->value();
	}

	// This must be rational; we don't need a type check
	auto flonum = cell_unchecked_cast<FlonumCell>(numeric);

	if (!flonum->isInteger())
	{
		signalError(world, "Attempted to convert non-integral inexact rational to exact value", {numeric});
	}

	return static_cast<std::int64_t>(flonum->value());
}

double lliby_inexact(NumberCell *numeric)
{
	if (auto flonum = cell_cast<FlonumCell>(numeric))
	{
		// This is already inexact
		return flonum->value();
	}

	// This must be an exact int; we don't need a type check
	auto exactInt = cell_unchecked_cast<ExactIntegerCell>(numeric);

	// Cast to a double
	return static_cast<double>(exactInt->value());
}

bool lliby_is_rational(AnyCell *anyCell)
{
	if (ExactIntegerCell::isInstance(anyCell))
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

bool lliby_numeric_equal(NumberCell *value1, NumberCell *value2, ProperList<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 == value2; },
			[] (double value1, double value2) { return value1 == value2; });
}

bool lliby_numeric_lt(NumberCell *value1, NumberCell *value2, ProperList<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 < value2; },
			[] (double value1, double value2) { return value1 < value2; });
}

bool lliby_numeric_gt(NumberCell *value1, NumberCell *value2, ProperList<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 > value2; },
			[] (double value1, double value2) { return value1 > value2; });
}

bool lliby_numeric_lte(NumberCell *value1, NumberCell *value2, ProperList<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 <= value2; },
			[] (double value1, double value2) { return value1 <= value2; });
}

bool lliby_numeric_gte(NumberCell *value1, NumberCell *value2, ProperList<NumberCell> *argHead)
{
	return numericCompare(value1, value2, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 >= value2; },
			[] (double value1, double value2) { return value1 >= value2; });
}

NumberCell *lliby_max(World &world, NumberCell *currentMaxNumber, ProperList<NumberCell> *argHead)
{
	return selectNumericValue(world, currentMaxNumber, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 > value2; },
			[] (double value1, double value2) { return value1 > value2; });
}

NumberCell *lliby_min(World &world, NumberCell *currentMaxNumber, ProperList<NumberCell> *argHead)
{
	return selectNumericValue(world, currentMaxNumber, argHead,
			[] (std::int64_t value1, std::int64_t value2) { return value1 < value2; },
			[] (double value1, double value2) { return value1 < value2; });
}

}
