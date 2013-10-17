#ifndef _LLIBY_BINDING_BOXEDLISTELEMENT_H
#define _LLIBY_BINDING_BOXEDLISTELEMENT_H

#include "BoxedDatum.h"

#include <list>

namespace lliby
{

class BoxedListElement : public BoxedDatum
{
#include "generated/BoxedListElementMembers.h"
public:
	static const std::int64_t InvalidListLength = -1;

	static BoxedListElement *createProperList(const std::list<BoxedDatum*> &elements);
	static BoxedPair *createImproperList(const std::list<BoxedDatum*> &elements);

	std::int64_t listLength() const;

protected:
	explicit BoxedListElement(BoxedTypeId typeId) :
		BoxedDatum(typeId)
	{
	}

	BoxedListElement(BoxedTypeId typeId, GarbageState gcState) :
		BoxedDatum(typeId, gcState)
	{
	}
};

}

#endif
