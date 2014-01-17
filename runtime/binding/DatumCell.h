#ifndef _LLIBY_BINDING_DATUMCELL_H
#define _LLIBY_BINDING_DATUMCELL_H

#include <cstddef>

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
	void *operator new(std::size_t size)
	{
		return alloc::allocateCells(1);
	}

	void *operator new(size_t s, void *placement)
	{
		return placement;
	}

	// We're garbage collected; don't allow delete
	void operator delete(void *value) = delete;

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
	return T::fromDatum(datumValue);
}

template <class T>
const T* datum_cast(const DatumCell *datumValue)
{
	return T::fromDatum(datumValue);
}

template <>
inline DatumCell* datum_cast<DatumCell>(DatumCell *datumValue)
{
	return datumValue;
}

template <>
inline const DatumCell* datum_cast<DatumCell>(const DatumCell *datumValue)
{
	return datumValue;
}

}

#endif

