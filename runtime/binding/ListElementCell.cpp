#include "ListElementCell.h"

#include "PairCell.h"
#include "EmptyListCell.h"
#include "ProperList.h"

#include "alloc/RangeAlloc.h"

namespace lliby
{

AnyCell* ListElementCell::createList(World &world, std::vector<AnyCell*> &elements, AnyCell *tail)
{
	alloc::RangeAlloc allocation = alloc::allocateRange(world, elements.size());
	auto allocIt = allocation.end();

	auto it = elements.rbegin();
	AnyCell *cdr = tail;

	for(;it != elements.rend(); it++)
	{
		cdr = new (*--allocIt) PairCell(*it, cdr);
	}

	return cdr;
}

}
