#ifndef _LLIBY_BINDING_INEXACTRATIONALVALUE_H
#define _LLIBY_BINDING_INEXACTRATIONALVALUE_H

#include "BoxedDatum.h"

#include <cmath>
#include <limits>

namespace lliby
{

class InexactRationalValue : public BoxedDatum
{
#include "generated/InexactRationalValueMembers.h"
public:
	InexactRationalValue(double value) :
		BoxedDatum(BoxedTypeId::InexactRational),
		m_value(value)
	{
	}
	
	static InexactRationalValue *NaN()
	{
		return new InexactRationalValue(std::numeric_limits<double>::quiet_NaN());
	}

	static InexactRationalValue *positiveInfinity()
	{
		return new InexactRationalValue(std::numeric_limits<double>::infinity());
	}
	
	static InexactRationalValue *negativeInfinity()
	{
		return new InexactRationalValue(-std::numeric_limits<double>::infinity());
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
};

}

#endif
