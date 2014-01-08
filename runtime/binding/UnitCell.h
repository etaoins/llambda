#ifndef _LLIBY_BINDING_UNITCELL_H
#define _LLIBY_BINDING_UNITCELL_H

#include "SingletonValue.h"
#include "core/constinstances.h"

namespace lliby
{

class UnitCell : public SingletonValue<DatumCell>
{
#include "generated/UnitCellMembers.h"
public:
	UnitCell() :
		SingletonValue(CellTypeId::Unit)
	{
	}
	
	static const UnitCell* instance()
	{
		return &lliby_unit_value;
	}
};

}

#endif
