#include "PairValue.h"
#include "EmptyListValue.h"
#include "alloc/FreezeGc.h"

namespace lliby
{

BoxedDatum* PairValue::createProperList(const std::list<BoxedDatum*> &elements)
{
	// PairValue::cdr isn't const because it can be modified through the pair
	// However, there's nothing interesting to modify on EmptyList
	// Just cast the const away
	BoxedDatum *cdr = const_cast<EmptyListValue*>(EmptyListValue::instance()); 

	{
		alloc::FreezeGc freezer(elements.size());

		for(auto it = elements.rbegin(); it != elements.rend(); it++)
		{
			cdr = new PairValue(*it, cdr);
		}
	}

	return cdr;
}

PairValue* PairValue::createImproperList(const std::list<BoxedDatum*> &elements)
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

		auto cdr = new PairValue(secondLast, endValue);

		for(;it != elements.rend(); it++)
		{
			cdr = new PairValue(*it, cdr); 
		}
	
		return cdr;
	}
}

}
