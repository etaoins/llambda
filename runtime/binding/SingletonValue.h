#ifndef _LLIBY_BINDING_SINGLETONVALUE_H
#define _LLIBY_BINDING_SINGLETONVALUE_H

#include "DatumCell.h"

namespace lliby
{

template <class T>
class SingletonValue : public T
{
public:
	explicit SingletonValue(CellTypeId typeId) :
		// Don't attempt to collect this as garbage
		T(typeId, GarbageState::GlobalConstant)
	{
	}

	// Disallow heap creation to force singleton use
	void *operator new(size_t size) = delete;
};

}

#endif
