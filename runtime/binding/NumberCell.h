#ifndef _LLIBY_BINDING_NUMBERCELL_H
#define _LLIBY_BINDING_NUMBERCELL_H

#include "AnyCell.h"

namespace lliby
{

class NumberCell : public AnyCell
{
#include "generated/NumberCellMembers.h"
public:
	/**
	 * Converts this NumberCell in to a floating point value
	 */
	float toFloat() const;
	double toDouble() const;
	long double toLongDouble() const;

protected:
	explicit NumberCell(CellTypeId typeId, GarbageState gcState = GarbageState::HeapAllocatedCell) :
		AnyCell(typeId, gcState)
	{
	}
};

}

#endif
