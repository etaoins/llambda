#ifndef _LLIBY_BINDING_BOXEDSINGLETON_H
#define _LLIBY_BINDING_BOXEDSINGLETON_H

#include "BoxedDatum.h"

namespace lliby
{

class BoxedSingleton : public BoxedDatum
{
public:
	explicit BoxedSingleton(BoxedTypeId typeId) :
		// Don't attempt to collect this as garbage
		BoxedDatum(typeId, GarbageState::GlobalConstant)
	{
	}

	// Disallow heap creation to force singleton use
	void *operator new(size_t size) = delete;
};

}

#endif
