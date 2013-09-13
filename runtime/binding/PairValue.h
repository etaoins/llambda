#ifndef _LLIBY_BINDING_PAIRVALUE_H
#define _LLIBY_BINDING_PAIRVALUE_H

#include "BoxedDatum.h"

#include <list>

namespace lliby
{

class PairValue : public BoxedDatum
{
#include "generated/PairValueMembers.h"
public:
	PairValue(BoxedDatum *car, BoxedDatum *cdr) :
		BoxedDatum(BoxedTypeId::Pair),
		m_car(car),
		m_cdr(cdr)
	{
	}

	static BoxedDatum *createProperList(const std::list<BoxedDatum*> &elements);
	static PairValue *createImproperList(const std::list<BoxedDatum*> &elements);
};

}

#endif

