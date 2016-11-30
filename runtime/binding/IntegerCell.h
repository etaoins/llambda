#ifndef _LLIBY_BINDING_INTEGERCELL_H
#define _LLIBY_BINDING_INTEGERCELL_H

#include "NumberCell.h"

#include "alloc/allocator.h"
#include "core/constinstances.h"

namespace lliby
{

class IntegerCell : public NumberCell
{
#include "generated/IntegerCellMembers.h"
public:
	explicit IntegerCell(std::int64_t value, GarbageState gcState = GarbageState::AllocatedCell) :
		NumberCell(CellTypeId::Integer, gcState),
		m_value(value)
	{
	}

	static IntegerCell* fromValue(World &world, std::int64_t value)
	{
		if ((value >= 0) && (value < SmallIntegerCount))
		{
			return const_cast<IntegerCell*>(&llcore_small_integer_values[value]);
		}

		void *cellLocation = alloc::allocateCells(world);
		return new (cellLocation) IntegerCell(value);
	}
};

}

#endif
