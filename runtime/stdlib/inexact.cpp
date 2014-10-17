#include "binding/FlonumCell.h"

using namespace lliby;

extern "C"
{

bool lliby_is_finite(NumberCell *value)
{
	if (auto flonum = cell_cast<FlonumCell>(value))
	{
		return std::isfinite(flonum->value());
	}
	else
	{
		// Exact integers must be finite
		return true;
	}
}

bool lliby_is_infinite(NumberCell *value)
{
	if (auto flonum = cell_cast<FlonumCell>(value))
	{
		return std::isinf(flonum->value());
	}
	else
	{
		// Exact integers cannot be infinite
		return false;
	}
}

}
