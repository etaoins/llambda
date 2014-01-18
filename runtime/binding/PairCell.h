#ifndef _LLIBY_BINDING_PAIRCELL_H
#define _LLIBY_BINDING_PAIRCELL_H

#include "ListElementCell.h"

namespace lliby
{

class PairCell : public ListElementCell
{
#include "generated/PairCellMembers.h"
public:
	PairCell(DatumCell *car, DatumCell *cdr) :
		ListElementCell(CellTypeId::Pair),
		m_car(car),
		m_cdr(cdr)
	{
	}

	void setCar(DatumCell *obj)
	{
		m_car = obj;
	}
	
	void setCdr(DatumCell *obj)
	{
		m_cdr = obj;
	}
	
	// These are used by the garbage collector to update the car and cdr pointers during compaction
	
	DatumCell** carRef()
	{
		return &m_car;
	}

	DatumCell** cdrRef()
	{
		return &m_cdr;
	}
};

}

#endif

