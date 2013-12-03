#ifndef _LLIBY_BINDING_NUMERICCELL_H
#define _LLIBY_BINDING_NUMERICCELL_H

#include "DatumCell.h"

namespace lliby
{

class NumericCell : public DatumCell
{
#include "generated/NumericCellMembers.h"
protected:
	explicit NumericCell(CellTypeId typeId) :
		DatumCell(typeId)
	{
	}
};

}

#endif
