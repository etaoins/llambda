#include "NumberCell.h"
#include "ExactIntegerCell.h"
#include "FlonumCell.h"

#include <limits>
#include <cassert>

namespace lliby
{
namespace
{
	/**
	 * Returns if the given floating point value is in the range of values that represent contiguous integer values
	 *
	 * The range for this is capped at the range of std::int64_t under the assumption that this is checking if a cast
	 * to that type is safe.  This is not portable; it assumes IEEE floating point on Unix-like systems
	 */
	template<typename T>
	bool withinContiguousIntegerRange(T testValue)
	{
		// XXX: How do we do this portably?
		if (sizeof(T) == 4)
		{
			const auto limitValue = 1L << 24; // 2^24
			return ((testValue >= -limitValue) && (testValue <= limitValue));
		}
		else if (sizeof(T) == 8)
		{
			const auto limitValue = 1UL << 53; // 2^53
			return ((testValue >= -limitValue) && (testValue <= limitValue));
		}
		else if (sizeof(T) >= 10)
		{
			// At this point we're limited by std::int64_t
			return ((testValue >= std::numeric_limits<std::int64_t>::min()) && 
					(testValue <= std::numeric_limits<std::int64_t>::max()));
		}
		else
		{
			assert("Unknown floating point type");
		}
	}
	
	template<typename T>
	NumberCell *floatToCell(World &world, T floatValue, bool canBeExact)
	{
		if (canBeExact && withinContiguousIntegerRange(floatValue)) 
		{
			std::int64_t integerResult = floatValue;

			if (integerResult == floatValue)
			{
				return ExactIntegerCell::fromValue(world, integerResult);
			}
		}

		return FlonumCell::fromValue(world, floatValue);
	}
	
	template<typename T>
	T cellToFloat(const NumberCell *value)
	{
		if (auto exactInteger = cell_cast<const ExactIntegerCell>(value))
		{
			return exactInteger->value();
		}
		else
		{
			auto flonum = cell_unchecked_cast<const FlonumCell>(value);
			return flonum->value();
		}
	}
}

NumberCell* NumberCell::fromValue(World &world, float value, bool canBeExact)
{
	return floatToCell(world, value, canBeExact);
}

NumberCell* NumberCell::fromValue(World &world, double value, bool canBeExact)
{
	return floatToCell(world, value, canBeExact);
}

NumberCell* NumberCell::fromValue(World &world, long double value, bool canBeExact)
{
	return floatToCell(world, value, canBeExact);
}

float NumberCell::toFloat() const
{
	return cellToFloat<float>(this);
}

double NumberCell::toDouble() const
{
	return cellToFloat<double>(this);
}

long double NumberCell::toLongDouble() const
{
	return cellToFloat<long double>(this);
}

bool NumberCell::isExact() const
{
	return ExactIntegerCell::isInstance(this);
}

}
