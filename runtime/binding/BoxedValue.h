#ifndef _LLIBY_BINDING_BOXEDVALUE_H
#define _LLIBY_BINDING_BOXEDVALUE_H

#include "generated/declaretypes.h"
#include "generated/typeid.h"

namespace lliby
{

class BoxedValue
{
#include "generated/BoxedValueMembers.h"
protected:
	BoxedValue(BoxedTypeId typeId) : m_typeId(typeId)
	{
	}
};

}

#endif

