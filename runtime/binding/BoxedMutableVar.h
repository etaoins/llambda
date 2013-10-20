#ifndef _LLIBY_BINDING_BOXEDMUTABLEVAR_H
#define _LLIBY_BINDING_BOXEDMUTABLEVAR_H

#include "BoxedDatum.h"

namespace lliby
{

class BoxedMutableVar : public BoxedDatum
{
#include "generated/BoxedMutableVarMembers.h"
public:
	BoxedMutableVar() : BoxedDatum(BoxedTypeId::MutableVar)
	{
	}
};

}

#endif
