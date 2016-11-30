#include "binding/NumberCell.h"
#include "binding/IntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/ProperList.h"
#include "binding/TypedPairCell.h"

#include <cmath>
#include <cfloat>
#include <iostream>
#include <limits>

#include "core/error.h"

using namespace lliby;

namespace
{
	// Helper used by llbase_div
	FlonumCell *flonumDivision(World &world, double startValue, ProperList<NumberCell>::Iterator begin, ProperList<NumberCell>::Iterator end)
	{
		double numeratorValue = startValue;

		for (auto it = begin; it != end; it++)
		{
			NumberCell *denominatorCell = *it;

			if (auto denomintorInteger = cell_cast<IntegerCell>(denominatorCell))
			{
				auto denominatorInt = denomintorInteger->value();

				if (denominatorInt == 0)
				{
					signalError(world, ErrorCategory::DivideByZero, "Attempted (/) by integer zero");
				}

				numeratorValue /= static_cast<double>(denominatorInt);
			}
			else
			{
				auto denomintorFlonum = cell_unchecked_cast<FlonumCell>(denominatorCell);
				numeratorValue /= denomintorFlonum->value();
			}
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

	bool integerDivisionWouldOverflow(std::int64_t num, std::int64_t denom)
	{
		return (num == std::numeric_limits<std::int64_t>::min()) && (denom == -1);
	}

	/**
	 * Determines if a float value can be exactly converted to the specified integer type
	 *
	 * This checks that the value is within the floating point type's contiguous integer range and that it would not
	 * overflow the target integer type.
	 */
	template<typename F, typename I>
	bool floatValueCanBeExactlyConverted(F value)
	{
		static_assert(!std::numeric_limits<I>::is_iec559, "Float conversion source must be a floating point type");
		static_assert(std::numeric_limits<I>::is_integer, "Float conversion target must an integer");

		auto floatRadixBits = std::numeric_limits<F>::digits;

		if (floatRadixBits >= std::numeric_limits<I>::digits)
		{
			// The floating point type can represent more contiguous integer values than the integer type
			return (value >= std::numeric_limits<I>::min()) &&
				   (value <= std::numeric_limits<I>::max());
		}
		else
		{
			// Ensure that we are within the floating point value's contiguous integer range
			return (value >= -(1LL << floatRadixBits)) &&
				   (value <= (1LL << floatRadixBits));
		}
	}
}

extern "C"
{

NumberCell *llbase_add(World &world, RestValues<NumberCell> *argList)
{
	std::int64_t integerSum = 0;
	double flonumSum = 0.0;
	bool resultIsFlonum = false;
	bool integerOverflowed = false;

	for (auto numeric : *argList)
	{
		if (auto integer = cell_cast<IntegerCell>(numeric))
		{
			long long nonOverflowSum;

			if (__builtin_saddll_overflow(integerSum, integer->value(), &nonOverflowSum))
			{
				// Convert to flonum and continue
				flonumSum += integer->value();
				integerOverflowed = true;
			}
			else
			{
				integerSum = nonOverflowSum;
			}
		}
		else
		{
			auto flonum = cell_unchecked_cast<FlonumCell>(numeric);

			flonumSum += flonum->value();
			resultIsFlonum = true;
		}
	}

	if (resultIsFlonum)
	{
		return FlonumCell::fromValue(world, integerSum + flonumSum);
	}
	else
	{
		if (integerOverflowed)
		{
			signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in (+)");
		}

		return IntegerCell::fromValue(world, integerSum);
	}
}

NumberCell *llbase_mul(World &world, RestValues<NumberCell> *argList)
{
	std::int64_t integerProduct = 1;
	double flonumProduct = 1.0;
	bool resultIsFlonum = false;
	bool integerOverflowed = false;

	for (auto numeric : *argList)
	{
		if (auto integer = cell_cast<IntegerCell>(numeric))
		{
			long long nonOverflowProduct;

			if (__builtin_smulll_overflow(integerProduct, integer->value(), &nonOverflowProduct))
			{
				// Convert to flonum and continue
				flonumProduct *= integer->value();
				integerOverflowed = true;
			}
			else
			{
				integerProduct = nonOverflowProduct;
			}
		}
		else
		{
			auto flonum = cell_unchecked_cast<FlonumCell>(numeric);

			flonumProduct *= flonum->value();
			resultIsFlonum = true;
		}
	}

	if (resultIsFlonum)
	{
		return FlonumCell::fromValue(world, integerProduct * flonumProduct);
	}
	else
	{
		if (integerOverflowed)
		{
			signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in (*)");
		}

		return IntegerCell::fromValue(world, integerProduct);
	}
}

NumberCell *llbase_sub(World &world, NumberCell *startValue, RestValues<NumberCell> *argList)
{
	std::int64_t integerDifference;
	double flonumDifference;
	bool resultIsFlonum;
	bool integerOverflowed = false;

	if (auto integer = cell_cast<IntegerCell>(startValue))
	{
		if (argList->empty())
		{
			// Return the inverse
			long long inverse;

			if (__builtin_ssubll_overflow(0LL, integer->value(), &inverse))
			{
				signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in inverting (-)");
			}

			return IntegerCell::fromValue(world, inverse);
		}

		integerDifference = integer->value();
		flonumDifference = 0.0;
		resultIsFlonum = false;
	}
	else
	{
		auto flonum = cell_unchecked_cast<FlonumCell>(startValue);

		if (argList->empty())
		{
			// Return the inverse
			return FlonumCell::fromValue(world, -flonum->value());
		}

		integerDifference = 0;
		flonumDifference = flonum->value();
		resultIsFlonum = true;
	}

	for (auto numeric : *argList)
	{
		if (auto integer = cell_cast<IntegerCell>(numeric))
		{
			long long nonOverflowDifference;

			if (__builtin_ssubll_overflow(integerDifference, integer->value(), &nonOverflowDifference))
			{
				// Convert to flonum and continue
				flonumDifference -= integer->value();
				integerOverflowed = true;
			}
			else
			{
				integerDifference = nonOverflowDifference;
			}
		}
		else
		{
			auto flonum = cell_unchecked_cast<FlonumCell>(numeric);

			flonumDifference -= flonum->value();
			resultIsFlonum = true;
		}
	}

	if (resultIsFlonum)
	{
		return FlonumCell::fromValue(world, integerDifference + flonumDifference);
	}
	else
	{
		if (integerOverflowed)
		{
			signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in subtracting (-)");
		}

		return IntegerCell::fromValue(world, integerDifference);
	}
}

NumberCell* llbase_div(World &world, NumberCell *startValue, RestValues<NumberCell> *argList)
{
	if (argList->empty())
	{
		// Return the reciprocal
		// This can only be an integer if startValue is an integer 1 or -1
		if (auto startIntCell = cell_cast<IntegerCell>(startValue))
		{
			std::int64_t startInt = startIntCell->value();

			if (startInt == 0)
			{
				signalError(world, ErrorCategory::DivideByZero, "Attempted reciprocal (/) by integer zero");
			}

			if ((startInt == 1) || (startInt == -1))
			{
				return startIntCell;
			}
		}

		// Perform flonum reciprocal
		return FlonumCell::fromValue(world, 1.0 / startValue->toDouble());
	}

	if (auto startIntCell = cell_cast<IntegerCell>(startValue))
	{
		// Perform integer division until we hit a flonum value
		std::int64_t numeratorInt = startIntCell->value();

		for (auto it = argList->begin(); it != argList->end(); it++)
		{
			if (auto denomintorInteger = cell_cast<IntegerCell>(*it))
			{
				// We have another integer!
				std::int64_t denominatorInt = denomintorInteger->value();

				// Check for divide by zero
				if (denominatorInt == 0)
				{
					signalError(world, ErrorCategory::DivideByZero, "Attempted (/) by integer zero");
				}

				// Does it divide exactly and is not an or overflow?
				if (!integerDivisionWouldOverflow(numeratorInt, denominatorInt) &&
					((numeratorInt % denominatorInt) == 0))
				{
					// Yes!
					numeratorInt = numeratorInt / denominatorInt;
				}
				else
				{
					// No; perform this division as flonum and pass the tail to flonumDivision()
					double flonumResult = static_cast<double>(numeratorInt) / static_cast<double>(denominatorInt);
					return flonumDivision(world, flonumResult, ++it, argList->end());
				}
			}
			else
			{
				// Nope, this is a flonum. Have flonumDivision() handle this value
				return flonumDivision(world, static_cast<double>(numeratorInt), it, argList->end());
			}
		}

		// We have an integer result somehow!
		return IntegerCell::fromValue(world, numeratorInt);
	}
	else
	{
		double startDouble = cell_unchecked_cast<FlonumCell>(startValue)->value();
		return flonumDivision(world, startDouble, argList->begin(), argList->end());
	}
}

TypedPairCell<IntegerCell, IntegerCell>* llbase_truncate_div(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, ErrorCategory::DivideByZero, "Attempted (truncate/) by zero");
	}

	if (integerDivisionWouldOverflow(numerator, denominator))
	{
		signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in (truncate/)");
	}

	auto quotient = numerator / denominator;
	auto remainder = numerator % denominator;

	return TypedPairCell<IntegerCell, IntegerCell>::emplaceValues(world, quotient, remainder);
}

std::int64_t llbase_truncate_quotient(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, ErrorCategory::DivideByZero, "Attempted (truncate-quotient) by zero");
	}

	if (integerDivisionWouldOverflow(numerator, denominator))
	{
		signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in (truncate-quotient)");
	}

	return numerator / denominator;
}

std::int64_t llbase_truncate_remainder(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, ErrorCategory::DivideByZero, "Attempted (truncate-remainder) by zero");
	}

	if (denominator == -1)
	{
		// Avoid integer overflow
		return 0;
	}

	return numerator % denominator;
}

TypedPairCell<IntegerCell, IntegerCell>* llbase_floor_div(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, ErrorCategory::DivideByZero, "Attempted (floor/) by zero");
	}

	if (integerDivisionWouldOverflow(numerator, denominator))
	{
		signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in (floor/)");
	}

	auto floorResult = floorDivision(numerator, denominator);
	return TypedPairCell<IntegerCell, IntegerCell>::emplaceValues(world, floorResult.quotient, floorResult.remainder);
}

std::int64_t llbase_floor_quotient(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, ErrorCategory::DivideByZero, "Attempted (floor-quotient) by zero");
	}

	if (integerDivisionWouldOverflow(numerator, denominator))
	{
		signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in (floor-quotient)");
	}

	return floorDivision(numerator, denominator).quotient;
}

std::int64_t llbase_floor_remainder(World &world, std::int64_t numerator, std::int64_t denominator)
{
	if (denominator == 0)
	{
		signalError(world, ErrorCategory::DivideByZero, "Attempted (floor-remainder) by zero");
	}

	if (denominator == -1)
	{
		// Avoid integer overflow
		return 0;
	}

	return floorDivision(numerator, denominator).remainder;
}

NumberCell* llbase_expt(World &world, NumberCell *base, NumberCell *power)
{
	const bool bothInteger = IntegerCell::isInstance(base) && IntegerCell::isInstance(power);

	if (bothInteger)
	{
		auto integerBase = static_cast<IntegerCell*>(base)->value();
		auto integerPower = static_cast<IntegerCell*>(power)->value();

		// Allow most powers of two to be exactly handled even on platforms with 64bit doubles
		if ((integerBase == 2) && (integerPower >= 0) && (integerPower <= 62))
		{
			return IntegerCell::fromValue(world, 1LL << integerPower);
		}
	}

	// Convert to long double to give us 80bits on x86-64 which allow us to have an extended range of exactly
	// represented integers
	const long double floatBase = base->toLongDouble();
	const long double floatPower = power->toLongDouble();

	const long double floatResult = powl(floatBase, floatPower);

	if (bothInteger)
	{
		if (!floatValueCanBeExactlyConverted<long double, std::int64_t>(floatResult))
		{
			signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in (expt)");
		}
		else
		{
			return IntegerCell::fromValue(world, floatResult);
		}
	}
	else
	{
		return FlonumCell::fromValue(world, floatResult);
	}
}

std::int64_t llbase_gcd(std::int64_t a, std::int64_t b, RestValues<IntegerCell> *restInts)
{
	std::int64_t result = greatestCommonDivisor(a, b);

	for(auto restInt : *restInts)
	{
		result = greatestCommonDivisor(result, restInt->value());
	}

	return (result < 0) ? -result : result;
}

std::int64_t llbase_lcm(std::int64_t a, std::int64_t b, RestValues<IntegerCell> *restInts)
{
	std::int64_t result = leastCommonMultiple(a, b);

	for(auto restInt : *restInts)
	{
		result = leastCommonMultiple(result, restInt->value());
	}

	return (result < 0) ? -result : result;
}

TypedPairCell<IntegerCell, IntegerCell>* llbase_integer_sqrt(World &world, std::int64_t val)
{
	if (val < 0)
	{
		signalError(world, ErrorCategory::Range, "Attempted (integer-sqrt) with negative value");
	}

	// This depends on the integral sqrt in C++11
	const std::int64_t floorResult = std::sqrt(val);
	const std::int64_t remainder = val - (floorResult * floorResult);

	return TypedPairCell<IntegerCell, IntegerCell>::emplaceValues(world, floorResult, remainder);
}

}
