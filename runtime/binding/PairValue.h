#ifndef _LLIBY_BINDING_PAIRVALUE_H
#define _LLIBY_BINDING_PAIRVALUE_H

#include "BoxedDatum.h"

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
};

}

#endif

