#include "BoxedListElement.h"

#include "alloc/FreezeGc.h"
#include "BoxedPair.h"
#include "BoxedEmptyList.h"

namespace lliby
{

BoxedListElement* BoxedListElement::createProperList(const std::list<BoxedDatum*> &elements)
{
	// BoxedListElement::cdr isn't const because it can be modified through the pair
	// However, there's nothing interesting to modify on EmptyList
	// Just cast the const away
	BoxedListElement *cdr = const_cast<BoxedEmptyList*>(BoxedEmptyList::instance()); 

	{
		alloc::FreezeGc freezer(elements.size());

		for(auto it = elements.rbegin(); it != elements.rend(); it++)
		{
			cdr = new BoxedPair(*it, cdr);
		}
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
	
	auto it = elements.rbegin();

	BoxedDatum *endValue = *(it++);
	BoxedDatum *secondLast = *(it++);

	{
		alloc::FreezeGc freezer(elements.size() - 1);

		auto cdr = new BoxedPair(secondLast, endValue);

		for(;it != elements.rend(); it++)
		{
			cdr = new BoxedPair(*it, cdr); 
		}
	
		return cdr;
	}
}

std::int64_t BoxedListElement::listLength() const
{
	const BoxedDatum *datum = this;
	std::uint32_t length = 0;

	while(auto pair = datum_cast<BoxedPair>(datum))
	{
		length++;
		datum = pair->cdr();
	}

	if (datum != BoxedEmptyList::instance())
	{
		// Not a list
		return InvalidListLength;
	}

	return length;
}

}
