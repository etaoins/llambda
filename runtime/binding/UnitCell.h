#ifndef _LLIBY_BINDING_UNITCELL_H
#define _LLIBY_BINDING_UNITCELL_H

#include "PreconstructedValue.h"
#include "core/constinstances.h"

namespace lliby
{

class UnitCell : public PreconstructedValue<AnyCell>
{
#include "generated/UnitCellMembers.h"
public:
	UnitCell() :
		PreconstructedValue(CellTypeId::Unit)
	{
	}

	static UnitCell* instance()
	{
		return const_cast<UnitCell*>(&llcore_unit_value);
	}
};

}

#endif
