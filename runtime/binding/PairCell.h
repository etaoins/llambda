#ifndef _LLIBY_BINDING_PAIRCELL_H
#define _LLIBY_BINDING_PAIRCELL_H

#include "ListElementCell.h"
#include <cassert>

namespace lliby
{
class World;

class PairCell : public ListElementCell
{
	friend class ListElementCell;
#include "generated/PairCellMembers.h"
public:
	PairCell(AnyCell *car, AnyCell *cdr) :
		ListElementCell(CellTypeId::Pair),
		m_car(car),
		m_cdr(cdr)
	{
	}

	/**
	 * Creates a new instance of PairCell 
	 *
	 * @param  world  World to allocate the pair in
	 * @param  car    car value of the new pair. This will be GC rooted internally.
	 * @param  cdr    cdr value of the new pair. This will be GC rooted internally.
	 */
	static PairCell* createInstance(World &world, AnyCell *car, AnyCell *cdr);

	void setCar(AnyCell *obj)
	{
		assert(!isGlobalConstant());
		m_car = obj;
	}
	
	void setCdr(AnyCell *obj)
	{
		assert(!isGlobalConstant());
		m_cdr = obj;
	}
	
	// These are used by the garbage collector to update the car and cdr pointers during compaction
	
	AnyCell** carRef()
	{
		return &m_car;
	}

	AnyCell** cdrRef()
	{
		return &m_cdr;
	}
};

}

#endif

