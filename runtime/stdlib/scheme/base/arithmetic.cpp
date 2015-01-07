#include "binding/NumberCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/ProperList.h"

#include <cmath>
#include <cfloat>
#include <iostream>
#include <limits>

#include "core/error.h"

using namespace lliby;

namespace
{
	// Helper used by llbase_div
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

	struct InexactFractionResult
	{
		double numerator;
		double denominator;
	};

	InexactFractionResult inexactFraction(double value)
	{
		// Radix == 2 lets us use bit shifts and bit counts which are more obviously correct and faster. Even on
		// platforms that support non-binary floating point it's highly doubtful the default double type would be
		// non-binary.
		static_assert(FLT_RADIX == 2, "Only radix of 2 is supported");

		// Handle these special cases explicitly
		if (std::isnan(value))
		{
			return {NAN, NAN};
		}
		else if (std::isinf(value))
		{
			return {value, 1.0};
		}
		else if (value == 0.0)
		{
			// Make sure we return value to catch -0.0
			return {value, 1.0};
		}

		int exponent;
		double significand = std::frexp(value, &exponent);

		if (exponent >= DBL_MANT_DIG)
		{
			// The value requires more range than a double's significand can represent without being magnified by an
			// exponent. This means the value must be an integer - we can return it directly
			return {value, 1};
		}
		else
		{
			// Multiply the significand in to an integer and reduce the exponent to match
			std::int64_t integerMantissa = significand * double(1ULL << DBL_MANT_DIG);
			exponent -= DBL_MANT_DIG;

			// Reduce the fraction - count the trailing zeros so we know how many times we can divide the significand
			// by 2. Limit the exponent to <= 0 so we don't create a fractional denominator.
			const int shiftRightBy = std::min(-exponent, __builtin_ctzll(integerMantissa));
			integerMantissa = integerMantissa >> shiftRightBy;
			exponent += shiftRightBy;

			return {static_cast<double>(integerMantissa), static_cast<double>(1ULL << -exponent)};
		}
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
}

extern "C"
{

NumberCell *llbase_add(World &world, RestValues<NumberCell> *argList)
{
	std::int64_t exactSum = 0;
	double inexactSum = 0.0;
	bool resultInexact = false;
	bool integerOverflowed = false;

	for (auto numeric : *argList)
	{
		if (auto exactInteger = cell_cast<ExactIntegerCell>(numeric))
		{
			long long nonOverflowSum;

			if (__builtin_saddll_overflow(exactSum, exactInteger->value(), &nonOverflowSum))
			{
				// Convert to inexact and continue
				inexactSum += exactInteger->value();
				integerOverflowed = true;
			}
			else
			{
				exactSum = nonOverflowSum;
			}
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
		if (integerOverflowed)
		{
			signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in (+)");
		}

		return ExactIntegerCell::fromValue(world, exactSum);
	}
}

NumberCell *llbase_mul(World &world, RestValues<NumberCell> *argList)
{
	std::int64_t exactProduct = 1;
	double inexactProduct = 1.0;
	bool resultInexact = false;
	bool integerOverflowed = false;

	for (auto numeric : *argList)
	{
		if (auto exactInteger = cell_cast<ExactIntegerCell>(numeric))
		{
			long long nonOverflowProduct;

			if (__builtin_smulll_overflow(exactProduct, exactInteger->value(), &nonOverflowProduct))
			{
				// Convert to inexact and continue
				inexactProduct *= exactInteger->value();
				integerOverflowed = true;
			}
			else
			{
				exactProduct = nonOverflowProduct;
			}
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
		if (integerOverflowed)
		{
			signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in (*)");
		}

		return ExactIntegerCell::fromValue(world, exactProduct);
	}
}

NumberCell *llbase_sub(World &world, NumberCell *startValue, RestValues<NumberCell> *argList)
{
	std::int64_t exactDifference;
	double inexactDifference;
	bool resultInexact;
	bool integerOverflowed = false;

	if (auto exactInteger = cell_cast<ExactIntegerCell>(startValue))
	{
		if (argList->empty())
		{
			// Return the inverse
			long long inverse;

			if (__builtin_ssubll_overflow(0LL, exactInteger->value(), &inverse))
			{
				signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in inverting (-)");
			}

			return ExactIntegerCell::fromValue(world, inverse);
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
			long long nonOverflowDifference;

			if (__builtin_ssubll_overflow(exactDifference, exactInteger->value(), &nonOverflowDifference))
			{
				// Convert to inexact and continue
				inexactDifference -= exactInteger->value();
				integerOverflowed = true;
			}
			else
			{
				exactDifference = nonOverflowDifference;
			}
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
		if (integerOverflowed)
		{
			signalError(world, ErrorCategory::IntegerOverflow, "Integer overflow in subtracting (-)");
		}

		return ExactIntegerCell::fromValue(world, exactDifference);
	}
}

NumberCell* llbase_div(World &world, NumberCell *startValue, RestValues<NumberCell> *argList)
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

ReturnValues<ExactIntegerCell>* llbase_truncate_div(World &world, std::int64_t numerator, std::int64_t denominator)
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

	return ReturnValues<ExactIntegerCell>::emplaceValues(world, {quotient, remainder});
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

ReturnValues<ExactIntegerCell>* llbase_floor_div(World &world, std::int64_t numerator, std::int64_t denominator)
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
	return ReturnValues<ExactIntegerCell>::emplaceValues(world, {floorResult.quotient, floorResult.remainder});
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
	const bool canBeExact = base->isExact() && power->isExact();

	// Convert to long double to give us 80bits on x86-64 which allow us to have an extended range of exactly
	// represented integers
	const long double floatBase = base->toLongDouble();
	const long double floatPower = power->toLongDouble();

	const long double floatResult = pow(floatBase, floatPower);

	return NumberCell::fromValue(world, floatResult, canBeExact);
}

std::int64_t llbase_gcd(std::int64_t a, std::int64_t b, RestValues<ExactIntegerCell> *restInts)
{
	std::int64_t result = greatestCommonDivisor(a, b);

	for(auto restInt : *restInts)
	{
		result = greatestCommonDivisor(result, restInt->value());
	}

	return (result < 0) ? -result : result;
}

std::int64_t llbase_lcm(std::int64_t a, std::int64_t b, RestValues<ExactIntegerCell> *restInts)
{
	std::int64_t result = leastCommonMultiple(a, b);

	for(auto restInt : *restInts)
	{
		result = leastCommonMultiple(result, restInt->value());
	}

	return (result < 0) ? -result : result;
}

ReturnValues<ExactIntegerCell>* llbase_exact_integer_sqrt(World &world, std::int64_t val)
{
	if (val < 0)
	{
		signalError(world, ErrorCategory::Range, "Attempted (exact-integer-sqrt) with negative value");
	}

	// This depends on the integral sqrt in C++11
	const std::int64_t floorResult = std::sqrt(val);
	const std::int64_t remainder = val - (floorResult * floorResult);

	return ReturnValues<ExactIntegerCell>::emplaceValues(world, {floorResult, remainder});
}

double llbase_numerator(double value)
{
	return inexactFraction(value).numerator;
}

double llbase_denominator(double value)
{
	return inexactFraction(value).denominator;
}

NumberCell* llbase_rationalize(World &world, NumberCell *valCell, double maxDiff)
{
	// If maxdiff is NaN then we are NaN
	if (std::isnan(maxDiff))
	{
		return FlonumCell::NaN(world);
	}
	else if (maxDiff < 0.0)
	{
		signalError(world, ErrorCategory::Range, "Attempted (rationalize) with negative maximum difference");
	}

	if (ExactIntegerCell::isInstance(valCell))
	{
		// Already an integer - this is in simplest terms
		return valCell;
	}

	const double floatVal = cell_unchecked_cast<FlonumCell>(valCell)->value();

	if (std::isnan(floatVal) || std::isinf(floatVal))
	{
		// Propagate these through directly
		return valCell;
	}

	InexactFractionResult fractionVal = inexactFraction(floatVal);

	struct CandidateResult
	{
		std::int64_t numerator;
		std::int64_t denominator;

		double toDouble() const
		{
			return static_cast<double>(numerator) / static_cast<double>(denominator);
		}
	};

	CandidateResult bestResult {
		static_cast<std::int64_t>(fractionVal.numerator),
		static_cast<std::int64_t>(fractionVal.denominator),
	};

	CandidateResult searchResult = bestResult;

	while(searchResult.denominator > 1)
	{
		// Try rounding the numerator both up and down. The denominator is a power of two so it always divides exactly.
		for(int rounder = 1; rounder >= 0; rounder--)
		{
			CandidateResult testResult {
				(searchResult.numerator / 2) + ((floatVal < 0) ? -rounder : rounder),
				searchResult.denominator / 2
			};

			const double absoluteDiff = std::fabs(testResult.toDouble() - floatVal);

			if (absoluteDiff <= maxDiff)
			{
				// This is allowed. Because we're testing progressively smaller fractions this is by definition the best
				bestResult = testResult;
			}

			searchResult = testResult;
		}
	}

	return FlonumCell::fromValue(world, bestResult.toDouble());
}

}
