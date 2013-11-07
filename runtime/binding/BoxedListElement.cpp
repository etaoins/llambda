#include "BoxedListElement.h"

#include "BoxedPair.h"
#include "BoxedEmptyList.h"

#include "alloc/RangeAlloc.h"

namespace lliby
{

BoxedListElement* BoxedListElement::createProperList(const std::list<BoxedDatum*> &elements)
{
	auto list = createList(elements, const_cast<BoxedEmptyList*>(BoxedEmptyList::instance()));

	return static_cast<BoxedListElement*>(list);
}

BoxedDatum* BoxedListElement::createList(const std::list<BoxedDatum*> &elements, BoxedDatum *tail)
{
	alloc::RangeAlloc allocation = alloc::allocateRange(elements.size());
	auto allocIt = allocation.end();
	
	auto it = elements.rbegin();
	BoxedDatum *cdr = tail;

	for(;it != elements.rend(); it++)
	{
		cdr = new (*--allocIt) BoxedPair(*it, cdr); 
	}

	return cdr;
}

}
