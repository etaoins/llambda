#ifndef _LLIBY_BINDING_BOXEDDATUM_H
#define _LLIBY_BINDING_BOXEDDATUM_H

#include "generated/declaretypes.h"
#include "generated/typeid.h"

namespace lliby
{

class BoxedDatum
{
#include "generated/BoxedDatumMembers.h"
protected:
	BoxedDatum(BoxedTypeId typeId) : m_typeId(typeId)
	{
	}
};

}

#endif

