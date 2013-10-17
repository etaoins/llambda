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

	void *operator new(size_t s, BoxedDatum *placement)
	{
		return placement;
	}

	// We're garbage collected; don't allow delete
	void operator delete(void *value) = delete;

protected:
	// Used for normal allocations
	// alloc::allocateCons already returns the correct garbage state
	BoxedDatum(BoxedTypeId typeId) : m_typeId(typeId)
	{
	}

	// Used for constant allocations
	BoxedDatum(BoxedTypeId typeId, GarbageState gcState) :
		m_typeId(typeId),
		m_gcState(gcState)
	{
	}
};

}

#endif

