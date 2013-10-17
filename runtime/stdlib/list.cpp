#include "binding/BoxedPair.h"
#include "binding/BoxedEmptyList.h"
#include "core/fatal.h"
#include "alloc/allocator.h"

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
	std::int64_t length = head->listLength();;

	if (length == BoxedListElement::InvalidListLength)
	{
		_lliby_fatal("Non-list passed to list-length", head);
	}

	return length;
}

BoxedListElement* lliby_make_list(std::uint32_t count, BoxedDatum *fill)
{
	if (count == 0)
	{
		return const_cast<BoxedEmptyList*>(BoxedEmptyList::instance());
	}

	// Allocate all the new pairs at once
	// This prevents us from having to pause GC
	BoxedPair *destHead = static_cast<BoxedPair*>(alloc::allocateCons(count));
	BoxedPair *destPair = destHead;

	// This will stop when there's one more pair left
	while(--count)
	{
		new (destPair) BoxedPair(fill, destPair + 1);
		destPair++;
	}
		
	// Terminate with the empty list
	new (destPair) BoxedPair(fill, const_cast<BoxedEmptyList*>(BoxedEmptyList::instance()));

	return destHead;
}

BoxedListElement* lliby_list_copy(const BoxedListElement *sourceHead)
{
	// Find the length of the list to copy
	const std::uint32_t listLength = sourceHead->listLength();

	if (listLength == 0)
	{
		// Nothing to do
		return const_cast<BoxedEmptyList*>(BoxedEmptyList::instance());
	}

	BoxedPair *destHead = static_cast<BoxedPair*>(alloc::allocateCons(listLength));
	BoxedPair *destPair = destHead;

	// Because we're a proper list this has to be a pair
	const BoxedPair *sourcePair = static_cast<const BoxedPair*>(sourceHead);

	while(true)
	{
		BoxedDatum *sourceCdr = sourcePair->cdr();

		if (sourceCdr == BoxedEmptyList::instance())
		{
			// Place our last pair cdr'ed to the empty list
			new (destPair) BoxedPair(sourcePair->car(), sourceCdr);

			// All done!
			return destHead;
		}
		else
		{
			// Create the new pair cdr'ed to the next pair
			new (destPair) BoxedPair(sourcePair->car(), destPair + 1);

			destPair++;

			// Move to the next pair
			sourcePair = static_cast<BoxedPair*>(sourceCdr);
		}
	}
}

}
