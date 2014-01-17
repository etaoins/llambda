#include "ListElementCell.h"

#include "PairCell.h"
#include "EmptyListCell.h"

#include "alloc/RangeAlloc.h"

namespace lliby
{

ListElementCell* ListElementCell::createProperList(const std::vector<DatumCell*> &elements)
{
	auto list = createList(elements, const_cast<EmptyListCell*>(EmptyListCell::instance()));

	return static_cast<ListElementCell*>(list);
}

DatumCell* ListElementCell::createList(const std::vector<DatumCell*> &elements, DatumCell *tail)
{
	alloc::RangeAlloc allocation = alloc::allocateRange(elements.size());
	auto allocIt = allocation.end();
	
	auto it = elements.rbegin();
	DatumCell *cdr = tail;

	for(;it != elements.rend(); it++)
	{
		cdr = new (*--allocIt) PairCell(*it, cdr); 
	}

	return cdr;
}

}
