#ifndef _LLIBY_BINDING_NUMERICVALUE_H
#define _LLIBY_BINDING_NUMERICVALUE_H

#include "BoxedDatum.h"

namespace lliby
{

class NumericValue : public BoxedDatum
{
#include "generated/NumericValueMembers.h"
protected:
	explicit NumericValue(BoxedTypeId typeId) :
		BoxedDatum(typeId)
	{
	}
};

}

#endif
