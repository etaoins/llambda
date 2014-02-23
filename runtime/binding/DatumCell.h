#ifndef _LLIBY_BINDING_DATUMCELL_H
#define _LLIBY_BINDING_DATUMCELL_H

#include <cstddef>
#include <cassert>

#include "alloc/GarbageState.h"
#include "alloc/allocator.h"

#include "generated/declaretypes.h"
#include "generated/typeid.h"

namespace lliby
{

class DatumCell
{
#include "generated/DatumCellMembers.h"
public:
	void *operator new(size_t s, void *placement)
	{
		return placement;
	}

	void operator delete(void *value)
	{
		// Since exceptions are enabled the compiler will wants to implicitly
		// call delete in some unwind paths. This isn't actually harmful as we'll
		// eventually garbage collect the cell.
	}

	/**
	 * Returns true if the other datum is equivalent to this one in the sense of
	 * eqv?
	 */
	bool isEqv(const DatumCell *other) const;
	
	/**
	 * Returns true if the other datum is equal to this one in the sense of
	 * equal?
	 */
	bool isEqual(const DatumCell *other) const;

	void setGcState(GarbageState gcState)
	{
		m_gcState = gcState;
	}

	void finalize();

protected:
	// Used for normal allocations
	// alloc::allocateCons already returns the correct garbage state
	DatumCell(CellTypeId typeId) : m_typeId(typeId)
	{
	}

	// Used for constant allocations
	DatumCell(CellTypeId typeId, GarbageState gcState) :
		m_typeId(typeId),
		m_gcState(gcState)
	{
	}
};

template <class T>
T* datum_cast(DatumCell *datumValue)
{
	if (T::isInstance(datumValue))
	{
		return static_cast<T*>(datumValue);
	}
	else
	{
		return nullptr;
	}
}

template <class T>
const T* datum_cast(const DatumCell *datumValue)
{
	if (T::isInstance(datumValue))
	{
		return static_cast<const T*>(datumValue);
	}
	else
	{
		return nullptr;
	}
}

template <class T>
T* datum_unchecked_cast(DatumCell *datumValue)
{
	// In debug builds make sure this is of the correct type
	assert(T::isInstance(datumValue));
	return static_cast<T*>(datumValue);
}

template <class T>
const T* datum_unchecked_cast(const DatumCell *datumValue)
{
	// In debug builds make sure this is of the correct type
	assert(T::isInstance(datumValue));
	return static_cast<T*>(datumValue);
}

template <>
inline DatumCell* datum_unchecked_cast<DatumCell>(DatumCell *datumValue)
{
	return datumValue;
}

template <>
inline const DatumCell* datum_unchecked_cast<DatumCell>(const DatumCell *datumValue)
{
	return datumValue;
}

}

#endif

