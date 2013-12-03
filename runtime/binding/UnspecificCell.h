#ifndef _LLIBY_BINDING_UNSPECIFICCELL_H
#define _LLIBY_BINDING_UNSPECIFICCELL_H

#include "SingletonValue.h"
#include "core/constinstances.h"

namespace lliby
{

class UnspecificCell : public SingletonValue<DatumCell>
{
#include "generated/UnspecificCellMembers.h"
public:
	UnspecificCell() :
		SingletonValue(CellTypeId::Unspecific)
	{
	}
	
	static const UnspecificCell* instance()
	{
		return &lliby_unspecific_value;
	}
};

}

#endif
