#ifndef _LLIBY_BINDING_UNSPECIFICVALUE_H
#define _LLIBY_BINDING_UNSPECIFICVALUE_H

#include "SingletonValue.h"
#include "core/constinstances.h"

namespace lliby
{

class UnspecificValue : public SingletonValue
{
#include "generated/UnspecificValueMembers.h"
public:
	UnspecificValue() :
		SingletonValue(BoxedTypeId::Unspecific)
	{
	}
	
	static const UnspecificValue* instance()
	{
		return &lliby_unspecific_value;
	}
};

}

#endif
