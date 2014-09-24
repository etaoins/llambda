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
	 * Converts a floating point value to a NumberCell instance
	 *
	 * @param  world       World to create the NumberCell in 
	 * @param  value       Value to convert to a NumberCell
	 * @param  canBeExact  True if an exact value can be created. This should only be true if the inputs to the
	 *                     computation that produced "value" only had exact inputs.
	 */
	static NumberCell* fromValue(World &world, float value, bool canBeExact);
	static NumberCell* fromValue(World &world, double value, bool canBeExact);
	static NumberCell* fromValue(World &world, long double value, bool canBeExact);

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
