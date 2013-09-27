#ifndef _LLIBY_BINDING_BOXEDPAIR_H
#define _LLIBY_BINDING_BOXEDPAIR_H

#include "BoxedDatum.h"

#include <list>

namespace lliby
{

class BoxedPair : public BoxedDatum
{
#include "generated/BoxedPairMembers.h"
public:
	BoxedPair(BoxedDatum *car, BoxedDatum *cdr) :
		BoxedDatum(BoxedTypeId::Pair),
		m_car(car),
		m_cdr(cdr)
	{
	}

	static BoxedDatum *createProperList(const std::list<BoxedDatum*> &elements);
	static BoxedPair *createImproperList(const std::list<BoxedDatum*> &elements);
};

}

#endif

