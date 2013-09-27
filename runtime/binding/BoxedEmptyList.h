#ifndef _LLIBY_BINDING_BOXEDEMPTYLIST_H
#define _LLIBY_BINDING_BOXEDEMPTYLIST_H

#include "BoxedSingleton.h"
#include "core/constinstances.h"

namespace lliby
{

class BoxedEmptyList : public BoxedSingleton
{
#include "generated/BoxedEmptyListMembers.h"
public:
	BoxedEmptyList() :
		BoxedSingleton(BoxedTypeId::EmptyList)
	{
	}
	
	static const BoxedEmptyList* instance()
	{
		return &lliby_empty_list_value;
	}
};

}

#endif
