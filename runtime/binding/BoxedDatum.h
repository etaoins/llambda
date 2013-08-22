#ifndef _LLIBY_BINDING_BOXEDDATUM_H
#define _LLIBY_BINDING_BOXEDDATUM_H

#include <cstddef>

#include "alloc/GarbageState.h"
#include "alloc/allocator.h"

#include "generated/declaretypes.h"
#include "generated/typeid.h"

namespace lliby
{

class BoxedDatum
{
#include "generated/BoxedDatumMembers.h"
public:
	void *operator new(std::size_t size)
	{
		return alloc::allocateCons();
	}

	// We're garbage collected; don't allow delete
	void operator delete(void *value) = delete;

protected:
	BoxedDatum(BoxedTypeId typeId) : m_typeId(typeId)
	{
	}
};

}

#endif

