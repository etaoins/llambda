#include "ListElementCell.h"

#include "PairCell.h"
#include "EmptyListCell.h"

#include "alloc/RangeAlloc.h"
#include "alloc/cellref.h"

namespace lliby
{

ListElementCell* ListElementCell::createProperList(World &world, std::vector<AnyCell*> &elements)
{
	auto list = createList(world, elements, EmptyListCell::instance());

	return cell_unchecked_cast<ListElementCell>(list);
}

AnyCell* ListElementCell::createList(World &world, std::vector<AnyCell*> &elements, AnyCell *tail)
{
	// We allocate space for our pairs below. Make sure we GC root the new elements first.
	alloc::AnyRefRange elementsRoot(world, elements);
	alloc::AnyRef tailRef(world, tail);

	alloc::RangeAlloc allocation = alloc::allocateRange(world, elements.size());
	auto allocIt = allocation.end();
	
	auto it = elements.rbegin();
	AnyCell *cdr = tailRef;

	for(;it != elements.rend(); it++)
	{
		cdr = new (*--allocIt) PairCell(*it, cdr); 
	}

	return cdr;
}

}
