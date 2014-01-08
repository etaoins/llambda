#ifndef _LLIBY_BINDING_EMPTYLISTCELL_H
#define _LLIBY_BINDING_EMPTYLISTCELL_H

#include "PreconstructedValue.h"
#include "ListElementCell.h"
#include "core/constinstances.h"

namespace lliby
{

class EmptyListCell : public PreconstructedValue<ListElementCell>
{
#include "generated/EmptyListCellMembers.h"
public:
	EmptyListCell() :
		PreconstructedValue(CellTypeId::EmptyList)
	{
	}
	
	static const EmptyListCell* instance()
	{
		return &lliby_empty_list_value;
	}
};

}

#endif
