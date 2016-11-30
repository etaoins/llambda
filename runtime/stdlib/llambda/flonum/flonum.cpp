#include "binding/FlonumCell.h"

using namespace lliby;

extern "C"
{

bool llflonum_is_finite(NumberCell *value)
{
	if (auto flonum = cell_cast<FlonumCell>(value))
	{
		return std::isfinite(flonum->value());
	}
	else
	{
		// Integers must be finite
		return true;
	}
}

bool llflonum_is_infinite(NumberCell *value)
{
	if (auto flonum = cell_cast<FlonumCell>(value))
	{
		return std::isinf(flonum->value());
	}
	else
	{
		// Integers cannot be infinite
		return false;
	}
}

}
