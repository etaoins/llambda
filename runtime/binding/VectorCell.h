#ifndef _LLIBY_BINDING_VECTORCELL_H
#define _LLIBY_BINDING_VECTORCELL_H

#include "AnyCell.h"

#include <vector>
#include <limits>
#include <algorithm>
#include <cassert>

namespace lliby
{

class VectorCell : public AnyCell
{
#include "generated/VectorCellMembers.h"
public:
	using LengthType = decltype(m_length);
	using SliceIndexType = std::int64_t;

	/**
	 * Creates a new VectorCell from the passed array of elements
	 */
	VectorCell(AnyCell **elements, LengthType length) :
		AnyCell(CellTypeId::Vector),
		m_length(length),
		m_elements(elements)
	{
	}

	constexpr static LengthType maximumLength()
	{
		// We're limited by our allocation size on both 32bit and 64bit
		return std::numeric_limits<std::size_t>::max() / sizeof(AnyCell*);
	}

	AnyCell* elementAt(LengthType offset) const
	{
		if (offset >= length())
		{
			return nullptr;
		}

		return elements()[offset];
	}

	bool setElementAt(LengthType offset, AnyCell *value)
	{
		if (offset >= length())
		{
			return false;
		}

		assert(!isGlobalConstant());
		elements()[offset] = value;

		return true;
	}

	/**
	 * Constructs a vector cell from the passed array of elements
	 *
	 * This transfers ownership of the elements array to the VectorCell. The array must be allocated using new[]
	 */
	static VectorCell* fromElements(World &world, AnyCell **elements, LengthType length);

	/**
	 * Creates a new vector with the given fill element
	 *
	 * If a the required memory cannot be allocated then nullptr is returned
	 */
	static VectorCell* fromFill(World &world, LengthType length, AnyCell *fill = nullptr);

	static VectorCell* fromAppended(World &world, const std::vector<const VectorCell*> &vectors);

	VectorCell* copy(World &world, SliceIndexType start = 0, SliceIndexType end = -1);
	bool replace(SliceIndexType offset, const VectorCell *from, SliceIndexType fromStart = 0, SliceIndexType fromEnd = -1);

	bool fill(AnyCell *fill, SliceIndexType start = 0, SliceIndexType end = -1);

	void finalizeVector();
};

}


#endif
