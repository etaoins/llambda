#ifndef _LLIBY_BINDING_LISTELEMENTCELL_H
#define _LLIBY_BINDING_LISTELEMENTCELL_H

#include "DatumCell.h"

#include <vector>

namespace lliby
{

class ListElementCell : public DatumCell
{
#include "generated/ListElementCellMembers.h"
public:
	static ListElementCell *createProperList(const std::vector<DatumCell*> &elements);
	static DatumCell *createList(const std::vector<DatumCell*> &elements, DatumCell *tail);

protected:
	explicit ListElementCell(CellTypeId typeId) :
		DatumCell(typeId)
	{
	}

	ListElementCell(CellTypeId typeId, GarbageState gcState) :
		DatumCell(typeId, gcState)
	{
	}
};

}

#endif
