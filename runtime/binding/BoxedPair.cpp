#include "BoxedPair.h"
#include "BoxedEmptyList.h"
#include "alloc/FreezeGc.h"

namespace lliby
{

BoxedDatum* BoxedPair::createProperList(const std::list<BoxedDatum*> &elements)
{
	// BoxedPair::cdr isn't const because it can be modified through the pair
	// However, there's nothing interesting to modify on EmptyList
	// Just cast the const away
	BoxedDatum *cdr = const_cast<BoxedEmptyList*>(BoxedEmptyList::instance()); 

	{
		alloc::FreezeGc freezer(elements.size());

		for(auto it = elements.rbegin(); it != elements.rend(); it++)
		{
			cdr = new BoxedPair(*it, cdr);
		}
	}

	return cdr;
}

BoxedPair* BoxedPair::createImproperList(const std::list<BoxedDatum*> &elements)
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

}
