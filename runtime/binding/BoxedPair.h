#ifndef _LLIBY_BINDING_BOXEDPAIR_H
#define _LLIBY_BINDING_BOXEDPAIR_H

#include "BoxedListElement.h"

namespace lliby
{

class BoxedPair : public BoxedListElement
{
#include "generated/BoxedPairMembers.h"
public:
	BoxedPair(BoxedDatum *car, BoxedDatum *cdr) :
		BoxedListElement(BoxedTypeId::Pair),
		m_car(car),
		m_cdr(cdr)
	{
	}

	void setCar(BoxedDatum *obj)
	{
		m_car = obj;
	}
	
	void setCdr(BoxedDatum *obj)
	{
		m_cdr = obj;
	}
};

}

#endif

