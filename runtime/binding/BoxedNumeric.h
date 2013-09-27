#ifndef _LLIBY_BINDING_BOXEDNUMERIC_H
#define _LLIBY_BINDING_BOXEDNUMERIC_H

#include "BoxedDatum.h"

namespace lliby
{

class BoxedNumeric : public BoxedDatum
{
#include "generated/BoxedNumericMembers.h"
protected:
	explicit BoxedNumeric(BoxedTypeId typeId) :
		BoxedDatum(typeId)
	{
	}
};

}

#endif
