#include "BoxedListElement.h"

#include "BoxedPair.h"
#include "BoxedEmptyList.h"

#include "alloc/RangeAlloc.h"

namespace lliby
{

BoxedListElement* BoxedListElement::createProperList(const std::list<BoxedDatum*> &elements)
{
	const size_t length = elements.size();

	alloc::RangeAlloc allocation = alloc::allocateRange(length);
	auto allocIt = allocation.end();

	BoxedListElement *cdr = const_cast<BoxedEmptyList*>(BoxedEmptyList::instance()); 

	for(auto it = elements.rbegin(); it != elements.rend(); it++)
	{
		cdr = new (*--allocIt) BoxedPair(*it, cdr);
	}

	return cdr;
}

BoxedPair* BoxedListElement::createImproperList(const std::list<BoxedDatum*> &elements)
{
	if (elements.size() < 2)
	{
		// Doesn't make sense
		return nullptr;
	}
	
	alloc::RangeAlloc allocation = alloc::allocateRange(elements.size() - 1);
	auto allocIt = allocation.end();
	
	auto it = elements.rbegin();

	BoxedDatum *endValue = *(it++);
	BoxedDatum *secondLast = *(it++);

	auto cdr = new BoxedPair(secondLast, endValue);

	for(;it != elements.rend(); it++)
	{
		cdr = new (*--allocIt) BoxedPair(*it, cdr); 
	}

	return cdr;
}

}
