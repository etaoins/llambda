#ifndef _LLIBY_BINDING_BOXEDEXACTINTEGER_H
#define _LLIBY_BINDING_BOXEDEXACTINTEGER_H

#include "BoxedNumeric.h"

namespace lliby
{

class BoxedExactInteger : public BoxedNumeric
{
#include "generated/BoxedExactIntegerMembers.h"
public:
	BoxedExactInteger(std::int64_t value) :
		BoxedNumeric(BoxedTypeId::ExactInteger),
		m_value(value)
	{
	}
};

}

#endif
