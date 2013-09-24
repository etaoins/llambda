#ifndef _LLIBY_BINDING_EXACTINTEGERVALUE_H
#define _LLIBY_BINDING_EXACTINTEGERVALUE_H

#include "NumericValue.h"

namespace lliby
{

class ExactIntegerValue : public NumericValue
{
#include "generated/ExactIntegerValueMembers.h"
public:
	static ExactIntegerValue* instanceForValue(std::int64_t value)
	{
		// XXX: Cache common integer values (-32 to 223?)
		return new ExactIntegerValue(value);
	}
	

private:
	ExactIntegerValue(std::int64_t value) :
		NumericValue(BoxedTypeId::ExactInteger),
		m_value(value)
	{
	}
};

}

#endif
