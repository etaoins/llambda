#include "ListElementCell.h"

#include "PairCell.h"
#include "EmptyListCell.h"

#include "alloc/RangeAlloc.h"
#include "alloc/StrongRef.h"

namespace lliby
{

ListElementCell* ListElementCell::createProperList(std::vector<DatumCell*> &elements)
{
	auto list = createList(elements, EmptyListCell::instance());

	return datum_unchecked_cast<ListElementCell>(list);
}

DatumCell* ListElementCell::createList(std::vector<DatumCell*> &elements, DatumCell *tail)
{
	// We allocate space for our pairs below. Make sure we GC root the new elements first.
	alloc::StrongRefRange<DatumCell> elementsRoot(elements);
	alloc::StrongRef<DatumCell> tailRef(tail);

	alloc::RangeAlloc allocation = alloc::allocateRange(elements.size());
	auto allocIt = allocation.end();
	
	auto it = elements.rbegin();
	DatumCell *cdr = tailRef;

	for(;it != elements.rend(); it++)
	{
		cdr = new (*--allocIt) PairCell(*it, cdr); 
	}

	return cdr;
}

}
