#include "binding/BoxedPair.h"
#include "binding/BoxedEmptyList.h"
#include "binding/ProperList.h"
#include "core/fatal.h"
#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"

using namespace lliby;

extern "C"
{

BoxedPair *lliby_cons(BoxedDatum *car, BoxedDatum *cdr)
{
	return new BoxedPair(car, cdr);
}

BoxedDatum *lliby_car(BoxedPair *pair)
{
	return pair->car();
}

BoxedDatum *lliby_cdr(BoxedPair *pair)
{
	return pair->cdr();
}

void lliby_set_car(BoxedPair *pair, BoxedDatum *obj)
{
	return pair->setCar(obj);
}

void lliby_set_cdr(BoxedPair *pair, BoxedDatum *obj)
{
	return pair->setCdr(obj);
}

std::uint32_t lliby_length(const BoxedListElement *head) 
{
	ProperList<BoxedDatum> properList(head);

	if (!properList.isValid())
	{
		_lliby_fatal("Non-list passed to list-length", head);
	}

	return properList.length();
}

BoxedListElement* lliby_make_list(std::uint32_t count, BoxedDatum *fill)
{
	BoxedListElement *cdr = const_cast<BoxedEmptyList*>(BoxedEmptyList::instance());

	// Allocate all the new pairs at once
	alloc::RangeAlloc allocation(alloc::allocateRange(count));
	auto allocIt = allocation.end();

	while(allocIt != allocation.begin())
	{
		cdr = new (*--allocIt) BoxedPair(fill, cdr);
	}

	return cdr;
}

BoxedListElement* lliby_list_copy(const BoxedListElement *sourceHead)
{
	// Find the number of pairs in the list
	// We can't use ProperList because we need to work with improper lists
	std::uint32_t pairCount = 0;

	for(auto pair = datum_cast<BoxedPair>(sourceHead);
		pair != nullptr;
		pair = datum_cast<BoxedPair>(pair->cdr()))
	{
		pairCount++;
	}

	if (pairCount == 0)
	{
		return const_cast<BoxedEmptyList*>(BoxedEmptyList::instance());
	}

	BoxedPair *destHead = static_cast<BoxedPair*>(alloc::allocateCons(pairCount));
	BoxedPair *destPair = destHead;

	// Because we're a proper list this has to be a pair
	const BoxedPair *sourcePair = static_cast<const BoxedPair*>(sourceHead);

	while(true)
	{
		BoxedDatum *sourceCdr = sourcePair->cdr();

		if (BoxedPair::isInstance(sourceCdr))
		{
			// Create the new pair cdr'ed to the next pair
			new (destPair) BoxedPair(sourcePair->car(), destPair + 1);

			destPair++;

			// Move to the next pair
			sourcePair = static_cast<BoxedPair*>(sourceCdr);
		}
		else
		{
			// Place our last pair cdr'ed to the last cdr
			// For proper lists this is the empty list
			// For improper list this is another type of non-pair datum
			new (destPair) BoxedPair(sourcePair->car(), sourceCdr);

			// All done!
			return destHead;
		}
	}
}

BoxedListElement* lliby_list(BoxedListElement *head)
{
	// Our calling convention requires that any rest parameters are passed as
	// a proper list. Because (list) is defined as only having rest args the
	// codegen will do the heavy lifting of building the list and we only have
	// to return it.
	return head;
}

}
