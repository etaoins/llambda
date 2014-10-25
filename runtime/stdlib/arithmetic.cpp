#include "binding/NumberCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/ProperList.h"
#include "binding/ReturnValuesList.h"

#include "alloc/RangeAlloc.h"

#include <cmath>

#include "core/error.h"

using namespace lliby;

namespace
{
	// Helper used by lliby_div
	FlonumCell *inexactDivision(World &world, double startValue, ProperList<NumberCell>::Iterator begin, ProperList<NumberCell>::Iterator end)
	{
		double numeratorValue = startValue;

		for (auto it = begin; it != end; it++)
		{
			numeratorValue /= (*it)->toDouble();
		}

		return FlonumCell::fromValue(world, numeratorValue);
	}

	// Helper used by the (floor/) procedures
	struct FloorDivisionResult
	{
		std::int64_t quotient;
		std::int64_t remainder;
	};

	FloorDivisionResult floorDivision(std::int64_t numerator, std::int64_t denominator)
	{
		// Do a truncating division first - this is the only integer division supported by C++
		std::int64_t quotient = numerator / denominator;
		std::int64_t remainder = numerator % denominator;

		if ((quotient < 0) && (remainder != 0))
		{
			// Fall down to the previous value
			quotient--;
			remainder += denominator;
		}

		return {quotient, remainder};
	}

	std::int64_t greatestCommonDivisor(std::int64_t a, std::int64_t b)
	{
		if (b == 0)
		{
			return a;
		}
		else
		{
			return greatestCommonDivisor(b, a % b);
		}
	}

	std::int64_t leastCommonMultiple(std::int64_t a, std::int64_t b)
	{
		const std::int64_t product = a * b;
		const std::int64_t gcm = greatestCommonDivisor(a, b);

		return product / gcm;
	}
}

extern "C"
{

NumberCell *lliby_add(World &world, ProperList<NumberCell> *argList)
{
	std::int64_t exactSum = 0;
	double inexactSum = 0.0;
	bool resultInexact = false;

	for (auto numeric : *argList)
	{
		if (auto exactInteger = cell_cast<ExactIntegerCell>(numeric))
		{
			exactSum += exactInteger->value();
		}
		else
		{
			auto flonum = cell_unchecked_cast<FlonumCell>(numeric);

			inexactSum += flonum->value();
			resultInexact = true;
		}
	}

	if (resultInexact)
	{
		return FlonumCell::fromValue(world, exactSum + inexactSum);
	}
	else
	{
		return ExactIntegerCell::fromValue(world, exactSum);
	}
}

NumberCell *lliby_mul(World &world, ProperList<NumberCell> *argList)
{
	std::int64_t exactProduct = 1;
	double inexactProduct = 1.0;
	bool resultInexact = false;

	for (auto numeric : *argList)
	{
		if (auto exactInteger = cell_cast<ExactIntegerCell>(numeric))
		{
			exactProduct *= exactInteger->value();
		}
		else
		{
			auto flonum = cell_unchecked_cast<FlonumCell>(numeric);

			inexactProduct *= flonum->value();
			resultInexact = true;
		}
	}

	if (resultInexact)
	{
		return FlonumCell::fromValue(world, exactProduct * inexactProduct);
	}
	else
	{
		return ExactIntegerCell::fromValue(world, exactProduct);
	}
}

NumberCell *lliby_sub(World &world, NumberCell *startValue, ProperList<NumberCell> *argList)
{
	std::int64_t exactDifference;
	double inexactDifference;
	bool resultInexact;

	if (auto exactInteger = cell_cast<ExactIntegerCell>(startValue))
	{
		if (argList->empty())
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
		auto flonum = cell_unchecked_cast<FlonumCell>(startValue);

		if (argList->empty())
		{
			// Return the inverse
			return FlonumCell::fromValue(world, -flonum->value());
		}

		exactDifference = 0;
		inexactDifference = flonum->value();
		resultInexact = true;
	}

	for (auto numeric : *argList)
	{
		if (auto exactInteger = cell_cast<ExactIntegerCell>(numeric))
		{
			exactDifference -= exactInteger->value();
		}
		else
		{
			auto flonum = cell_unchecked_cast<FlonumCell>(numeric);

			inexactDifference -= flonum->value();
			resultInexact = true;
		}
	}

	if (resultInexact)
	{
		return FlonumCell::fromValue(world, exactDifference + inexactDifference);
	}
	else
	{
		return ExactIntegerCell::fromValue(world, exactDifference);
	}
}

NumberCell* lliby_div(World &world, NumberCell *startValue, ProperList<NumberCell> *argList)
{
	if (argList->empty())
	{
		// Return the reciprocal
		// This can only be exact if startValue is an exact 1 or -1
		if (auto startExactInt = cell_cast<ExactIntegerCell>(startValue))
		{
			if ((startExactInt->value() == 1) || (startExactInt->value() == -1))
			{
				return startExactInt;
			}
		}

		// Perform inexact reciprocal
		return FlonumCell::fromValue(world, 1.0 / startValue->toDouble());
	}

	if (auto startExactInt = cell_cast<ExactIntegerCell>(startValue))
	{
		// Perform integer division until we hit an inexact value
		std::int64_t numeratorInt = startExactInt->value();

		for (auto it = argList->begin(); it != argList->end(); it++)
		{
			if (auto denomintorExactInt = cell_cast<ExactIntegerCell>(*it))
			{
				// We have another integer!
				std::int64_t denominatorInt = denomintorExactInt->value();

				// Does it divide exactly and is not a divide by zero?
				if ((denominatorInt != 0) && ((numeratorInt % denominatorInt) == 0))
				{
					// Yes!
					numeratorInt = numeratorInt / denominatorInt;
				}
				else
				{
					// No; perform this division as inexact and pass the tail to inexactDivision()
					double inexactResult = static_cast<double>(numeratorInt) / static_cast<double>(denominatorInt);
					return inexactDivision(world, inexactResult, ++it, argList->end());
				}
			}
			else
			{
				// Nope, this is a flonum. Have inexactDivision() handle this value
				return inexactDivision(world, static_cast<double>(numeratorInt), it, argList->end());
			}
		}

		// We have an exact result somehow!
		return ExactIntegerCell::fromValue(world, numeratorInt);
	}
	else
	{
		double startDouble = cell_unchecked_cast<FlonumCell>(startValue)->value();
		return inexactDivision(world, startDouble, argList->begin(), argList->end());
	}
}

ReturnValuesList* lliby_truncate_div(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, "Attempted (truncate/) by zero");
	}

	alloc::RangeAlloc allocation = alloc::allocateRange(world, 2);
	auto allocIt = allocation.begin();

	auto quotient = new (*allocIt++) ExactIntegerCell(numerator / denominator);
	auto remainder = new (*allocIt++) ExactIntegerCell(numerator % denominator);

	return ReturnValuesList::create(world, {quotient, remainder});
}

std::int64_t lliby_truncate_quotient(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, "Attempted (truncate-quotient) by zero");
	}

	return numerator / denominator;
}

std::int64_t lliby_truncate_remainder(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, "Attempted (truncate-remainder) by zero");
	}

	return numerator % denominator;
}

ReturnValuesList* lliby_floor_div(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, "Attempted (floor/) by zero");
	}

	auto floorResult = floorDivision(numerator, denominator);

	alloc::RangeAlloc allocation = alloc::allocateRange(world, 2);
	auto allocIt = allocation.begin();

	auto quotientCell = new (*allocIt++) ExactIntegerCell(floorResult.quotient);
	auto remainderCell = new (*allocIt++) ExactIntegerCell(floorResult.remainder);

	return ReturnValuesList::create(world, {quotientCell, remainderCell});
}

std::int64_t lliby_floor_quotient(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, "Attempted (truncate-quotient) by zero");
	}

	return floorDivision(numerator, denominator).quotient;
}

std::int64_t lliby_floor_remainder(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, "Attempted (truncate-remainder) by zero");
	}

	return floorDivision(numerator, denominator).remainder;
}

NumberCell* lliby_expt(World &world, NumberCell *base, NumberCell *power)
{
	const bool canBeExact = base->isExact() && power->isExact();

	// Convert to long double to give us 80bits on x86-64 which allow us to have an extended range of exactly
	// represented integers
	const long double floatBase = base->toLongDouble();
	const long double floatPower = power->toLongDouble();

	const long double floatResult = pow(floatBase, floatPower);

	return NumberCell::fromValue(world, floatResult, canBeExact);
}

std::int64_t lliby_gcd(std::int64_t a, std::int64_t b, ProperList<ExactIntegerCell> *restInts)
{
	std::int64_t result = greatestCommonDivisor(a, b);

	for(auto restInt : *restInts)
	{
		result = greatestCommonDivisor(result, restInt->value());
	}

	return (result < 0) ? -result : result;
}

std::int64_t lliby_lcm(std::int64_t a, std::int64_t b, ProperList<ExactIntegerCell> *restInts)
{
	std::int64_t result = leastCommonMultiple(a, b);

	for(auto restInt : *restInts)
	{
		result = leastCommonMultiple(result, restInt->value());
	}

	return (result < 0) ? -result : result;
}

ReturnValuesList* lliby_exact_integer_sqrt(World &world, std::int64_t val)
{
	if (val < 0)
	{
		signalError(world, "Attempted (exact-integer-sqrt) with negative value");
	}

	// This depends on the integral sqrt in C++11
	const std::int64_t floorResult = std::sqrt(val);
	const std::int64_t remainder = val - (floorResult * floorResult);

	alloc::RangeAlloc allocation = alloc::allocateRange(world, 2);
	auto allocIt = allocation.begin();

	auto floorResultCell = new (*allocIt++) ExactIntegerCell(floorResult);
	auto remainderCell = new (*allocIt++) ExactIntegerCell(remainder);

	return ReturnValuesList::create(world, {floorResultCell, remainderCell});
}

}
