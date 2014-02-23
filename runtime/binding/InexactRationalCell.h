#ifndef _LLIBY_BINDING_INEXACTRATIONALCELL_H
#define _LLIBY_BINDING_INEXACTRATIONALCELL_H

#include "NumericCell.h"

#include "alloc/allocator.h"

#include <cmath>
#include <limits>

namespace lliby
{

class InexactRationalCell : public NumericCell
{
#include "generated/InexactRationalCellMembers.h"
public:
	static InexactRationalCell* fromValue(World &world, double value)
	{
		void *cellPlacement = alloc::allocateCells(world);
		return new (cellPlacement) InexactRationalCell(value);
	}
	
	static InexactRationalCell *NaN(World &world)
	{
		void *cellPlacement = alloc::allocateCells(world);
		return new (cellPlacement) InexactRationalCell(std::numeric_limits<double>::quiet_NaN());
	}

	static InexactRationalCell *positiveInfinity(World &world)
	{
		void *cellPlacement = alloc::allocateCells(world);
		return new (cellPlacement) InexactRationalCell(std::numeric_limits<double>::infinity());
	}
	
	static InexactRationalCell *negativeInfinity(World &world)
	{
		void *cellPlacement = alloc::allocateCells(world);
		return new (cellPlacement) InexactRationalCell(-std::numeric_limits<double>::infinity());
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

	bool isInteger() const
	{
		double unused;
		return std::modf(value(), &unused) == 0.0;
	}
	
private:
	InexactRationalCell(double value) :
		NumericCell(CellTypeId::InexactRational),
		m_value(value)
	{
	}
};

}

#endif
