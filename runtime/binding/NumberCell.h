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

	/**
	 * Returns if this number is an exact value
	 *
	 * See R7RS for further discussion of exact values. Our implementation currently only supports exact integers
	 */
	bool isExact() const;

protected:
	explicit NumberCell(CellTypeId typeId) :
		AnyCell(typeId)
	{
	}
};

}

#endif
