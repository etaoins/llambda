#ifndef _LLIBY_BINDING_EOFOBJECTCELL_H
#define _LLIBY_BINDING_EOFOBJECTCELL_H

#include "AnyCell.h"
#include "PreconstructedValue.h"
#include "core/constinstances.h"

namespace lliby
{

class EofObjectCell : public PreconstructedValue<AnyCell>
{
#include "generated/EofObjectCellMembers.h"
public:
	EofObjectCell() :
		PreconstructedValue(CellTypeId::EofObject)
	{
	}

	static EofObjectCell* instance()
	{
		return const_cast<EofObjectCell*>(&lliby_eof_object_value);
	}
};

}

#endif
