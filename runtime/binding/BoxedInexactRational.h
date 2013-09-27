#ifndef _LLIBY_BINDING_BOXEDINEXACTRATIONAL_H
#define _LLIBY_BINDING_BOXEDINEXACTRATIONAL_H

#include "BoxedNumeric.h"

#include <cmath>
#include <limits>

namespace lliby
{

class BoxedInexactRational : public BoxedNumeric
{
#include "generated/BoxedInexactRationalMembers.h"
public:
	BoxedInexactRational(double value) :
		BoxedNumeric(BoxedTypeId::InexactRational),
		m_value(value)
	{
	}
	
	static BoxedInexactRational *NaN()
	{
		return new BoxedInexactRational(std::numeric_limits<double>::quiet_NaN());
	}

	static BoxedInexactRational *positiveInfinity()
	{
		return new BoxedInexactRational(std::numeric_limits<double>::infinity());
	}
	
	static BoxedInexactRational *negativeInfinity()
	{
		return new BoxedInexactRational(-std::numeric_limits<double>::infinity());
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
