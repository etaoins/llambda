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
	
	static EmptyListCell* instance()
	{
		// There is nothing inside EmptyListCell which is modifiable so const doesn't mean anything
		// Be friendly and allow functions expecting non-const to work with the empty list singleton without sprinkling
		// const casts throughout the code
		return const_cast<EmptyListCell*>(&lliby_empty_list_value);
	}
};

}

#endif
