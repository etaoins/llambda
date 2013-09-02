#ifndef _LLIBY_BINDING_SINGLETONVALUE_H
#define _LLIBY_BINDING_SINGLETONVALUE_H

#include "BoxedDatum.h"

namespace lliby
{

class SingletonValue : public BoxedDatum
{
public:
	explicit SingletonValue(BoxedTypeId typeId) :
		// Don't attempt to collect this as garbage
		BoxedDatum(typeId, GarbageState::GlobalConstant)
	{
	}

	// Disallow heap creation to force singleton use
	void *operator new(size_t size) = delete;
};

}

#endif
