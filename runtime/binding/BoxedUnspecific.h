#ifndef _LLIBY_BINDING_BOXEDUNSPECIFIC_H
#define _LLIBY_BINDING_BOXEDUNSPECIFIC_H

#include "BoxedSingleton.h"
#include "core/constinstances.h"

namespace lliby
{

class BoxedUnspecific : public BoxedSingleton
{
#include "generated/BoxedUnspecificMembers.h"
public:
	BoxedUnspecific() :
		BoxedSingleton(BoxedTypeId::Unspecific)
	{
	}
	
	static const BoxedUnspecific* instance()
	{
		return &lliby_unspecific_value;
	}
};

}

#endif
