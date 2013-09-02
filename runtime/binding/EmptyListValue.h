#ifndef _LLIBY_BINDING_EMPTYLISTVALUE_H
#define _LLIBY_BINDING_EMPTYLISTVALUE_H

#include "SingletonValue.h"
#include "core/constinstances.h"

namespace lliby
{

class EmptyListValue : public SingletonValue
{
#include "generated/EmptyListValueMembers.h"
public:
	EmptyListValue() :
		SingletonValue(BoxedTypeId::EmptyList)
	{
	}
	
	static const EmptyListValue* instance()
	{
		return &lliby_empty_list_value;
	}
};

}

#endif
