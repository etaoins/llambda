#ifndef _LLIBY_BINDING_EXACTINTEGERCELL_H
#define _LLIBY_BINDING_EXACTINTEGERCELL_H

#include "NumericCell.h"

#include "alloc/allocator.h"

namespace lliby
{

class ExactIntegerCell : public NumericCell
{
#include "generated/ExactIntegerCellMembers.h"
public:
	static ExactIntegerCell* fromValue(World &world, std::int64_t value)
	{
		void *cellLocation = alloc::allocateCells(world);
		return new (cellLocation) ExactIntegerCell(value);
	}

private:
	ExactIntegerCell(std::int64_t value) :
		NumericCell(CellTypeId::ExactInteger),
		m_value(value)
	{
	}
};

}

#endif
