#ifndef _LLIBY_BINDING_MUTABLEVARCELL_H
#define _LLIBY_BINDING_MUTABLEVARCELL_H

#include "DatumCell.h"

namespace lliby
{

class MutableVarCell : public DatumCell
{
#include "generated/MutableVarCellMembers.h"
public:
	MutableVarCell() : DatumCell(CellTypeId::MutableVar)
	{
	}
};

}

#endif
