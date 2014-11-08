#include "ListElementCell.h"

#include "PairCell.h"
#include "EmptyListCell.h"
#include "ProperList.h"

#include "alloc/RangeAlloc.h"
#include "alloc/cellref.h"

namespace lliby
{

namespace
{
	template<class Container>
	AnyCell* createListFromGcRooted(World &world, Container &elements, alloc::AnyRef &tailRef)
	{
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

AnyCell* ListElementCell::createList(World &world, alloc::StrongRefVector<AnyCell> &elements, AnyCell *tail)
{
	// We allocate space for our pairs below. Make sure we GC root the tail first.
	alloc::AnyRef tailRef(world, tail);
	return createListFromGcRooted(world, elements, tailRef);
}

AnyCell* ListElementCell::createList(World &world, std::vector<AnyCell*> &elements, AnyCell *tail)
{
	alloc::StrongRoot<AnyCell> elementsRoot(world, elements.data(), elements.size());
	alloc::AnyRef tailRef(world, tail);

	return createListFromGcRooted(world, elements, tailRef);
}

}
