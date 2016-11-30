#include "NumberCell.h"
#include "IntegerCell.h"
#include "FlonumCell.h"

#include <limits>
#include <cassert>

namespace lliby
{
namespace
{
	template<typename T>
	T cellToFloat(const NumberCell *value)
	{
		if (auto integer = cell_cast<const IntegerCell>(value))
		{
			return integer->value();
		}
		else
		{
			auto flonum = cell_unchecked_cast<const FlonumCell>(value);
			return flonum->value();
		}
	}
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

}
