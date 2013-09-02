#ifndef _LLIBY_BINDING_BOOLEANVALUE_H
#define _LLIBY_BINDING_BOOLEANVALUE_H

#include "SingletonValue.h"
#include "core/constinstances.h"

namespace lliby
{

class BooleanValue : public SingletonValue
{
#include "generated/BooleanValueMembers.h"
public:
	explicit BooleanValue(bool value) :
		SingletonValue(BoxedTypeId::Boolean),
		m_value(value)
	{
	}
	
	static const BooleanValue* falseInstance()
	{
		return &lliby_false_value;
	}

	static const BooleanValue* trueInstance()
	{
		return &lliby_true_value;
	}
};

}

#endif

