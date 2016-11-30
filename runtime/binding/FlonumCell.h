#ifndef _LLIBY_BINDING_FLONUMCELL_H
#define _LLIBY_BINDING_FLONUMCELL_H

#include "NumberCell.h"

#include "alloc/allocator.h"

#include <cmath>
#include <limits>

namespace lliby
{

class FlonumCell : public NumberCell
{
#include "generated/FlonumCellMembers.h"
public:
	explicit FlonumCell(double value) :
		NumberCell(CellTypeId::Flonum),
		m_value(value)
	{
	}

	static FlonumCell* fromValue(World &world, double value)
	{
		void *cellPlacement = alloc::allocateCells(world);
		return new (cellPlacement) FlonumCell(value);
	}
	
	static FlonumCell *NaN(World &world)
	{
		void *cellPlacement = alloc::allocateCells(world);
		return new (cellPlacement) FlonumCell(std::numeric_limits<double>::quiet_NaN());
	}

	static FlonumCell *positiveInfinity(World &world)
	{
		void *cellPlacement = alloc::allocateCells(world);
		return new (cellPlacement) FlonumCell(std::numeric_limits<double>::infinity());
	}
	
	static FlonumCell *negativeInfinity(World &world)
	{
		void *cellPlacement = alloc::allocateCells(world);
		return new (cellPlacement) FlonumCell(-std::numeric_limits<double>::infinity());
	}

	bool isNaN() const
	{
		return std::isnan(value());
	}

	bool isInfinite() const
	{
		return std::isinf(value());
	}

	bool isPositiveInfinity() const
	{
		return isInfinite() && (value() > 0);
	}
	
	bool isNegativeInfinity() const
	{
		return isInfinite() && (value() < 0);
	}

	bool isIntegral() const
	{
		double unused;
		return std::modf(value(), &unused) == 0.0;
	}
};

}

#endif
