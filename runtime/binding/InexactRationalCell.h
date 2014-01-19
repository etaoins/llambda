#ifndef _LLIBY_BINDING_INEXACTRATIONALCELL_H
#define _LLIBY_BINDING_INEXACTRATIONALCELL_H

#include "NumericCell.h"

#include <cmath>
#include <limits>

namespace lliby
{

class InexactRationalCell : public NumericCell
{
#include "generated/InexactRationalCellMembers.h"
public:
	static InexactRationalCell* fromValue(double value)
	{
		return new InexactRationalCell(value);
	}
	
	static InexactRationalCell *NaN()
	{
		return new InexactRationalCell(std::numeric_limits<double>::quiet_NaN());
	}

	static InexactRationalCell *positiveInfinity()
	{
		return new InexactRationalCell(std::numeric_limits<double>::infinity());
	}
	
	static InexactRationalCell *negativeInfinity()
	{
		return new InexactRationalCell(-std::numeric_limits<double>::infinity());
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
