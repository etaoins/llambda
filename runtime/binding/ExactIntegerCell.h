#ifndef _LLIBY_BINDING_EXACTINTEGERCELL_H
#define _LLIBY_BINDING_EXACTINTEGERCELL_H

#include "NumberCell.h"

#include "alloc/allocator.h"
#include "core/constinstances.h"

namespace lliby
{

class ExactIntegerCell : public NumberCell
{
#include "generated/ExactIntegerCellMembers.h"
public:
	explicit ExactIntegerCell(std::int64_t value, GarbageState gcState = GarbageState::AllocatedCell) :
		NumberCell(CellTypeId::ExactInteger, gcState),
		m_value(value)
	{
	}

	static ExactIntegerCell* fromValue(World &world, std::int64_t value)
	{
		if ((value >= 0) && (value < SmallExactIntegerCount))
		{
			return const_cast<ExactIntegerCell*>(&llcore_small_exact_integer_values[value]);
		}

		void *cellLocation = alloc::allocateCells(world);
		return new (cellLocation) ExactIntegerCell(value);
	}
};

}

#endif
