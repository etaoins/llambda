#ifndef _LLIBY_BINDING_NUMBERCELL_H
#define _LLIBY_BINDING_NUMBERCELL_H

#include "AnyCell.h"

namespace lliby
{

class NumberCell : public AnyCell
{
#include "generated/NumberCellMembers.h"
protected:
	explicit NumberCell(CellTypeId typeId) :
		AnyCell(typeId)
	{
	}
};

}

#endif
