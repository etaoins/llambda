#ifndef _LLIBY_BINDING_EXACTINTEGERCELL_H
#define _LLIBY_BINDING_EXACTINTEGERCELL_H

#include "NumberCell.h"

#include "alloc/allocator.h"

namespace lliby
{

class ExactIntegerCell : public NumberCell
{
#include "generated/ExactIntegerCellMembers.h"
public:
	explicit ExactIntegerCell(std::int64_t value) :
		NumberCell(CellTypeId::ExactInteger),
		m_value(value)
	{
	}

	static ExactIntegerCell* fromValue(World &world, std::int64_t value)
	{
		void *cellLocation = alloc::allocateCells(world);
		return new (cellLocation) ExactIntegerCell(value);
	}
};

}

#endif
