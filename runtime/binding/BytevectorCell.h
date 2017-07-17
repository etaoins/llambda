#ifndef _LLIBY_BINDING_BYTEVECTORCELL_H
#define _LLIBY_BINDING_BYTEVECTORCELL_H

#include "AnyCell.h"
#include "SharedByteArray.h"

#include <list>
#include <cassert>

namespace lliby
{

class BytevectorCell : public AnyCell
{
#include "generated/BytevectorCellMembers.h"
public:
	using LengthType = decltype(m_length);
	using SliceIndexType = std::int64_t;

	constexpr static LengthType maximumLength()
	{
		return (std::numeric_limits<LengthType>::max() < SharedByteArray::maximumCapacity()) ?
			std::numeric_limits<LengthType>::max() :
			SharedByteArray::maximumCapacity();
	}

	/**
	 * Creates a new BytevectorCell backed by the passed SharedByteArray
	 *
	 * The SharedByteArray must already have a reference taken for the bytevector cell
	 *
	 * @param  byteArray  Byte array to back the bytevector cell
	 * @param  length     Length of the data in bytes
	 */
	BytevectorCell(SharedByteArray *byteArray, LengthType length) :
		AnyCell(CellTypeId::Bytevector),
		m_length(length),
		m_byteArray(byteArray)
	{
	}

	/**
	 * Creates a new BytevectorCell backed by the passed SharedByteArray
	 *
	 * The SharedByteArray must already have a reference taken for the bytevector cell
	 *
	 * @param  world      World to create the bytevector cell in
	 * @param  byteArray  Byte array to back the bytevector cell
	 * @param  length     Length of the data in bytes
	 */
	static BytevectorCell* withByteArray(World &world, SharedByteArray *byteArray, LengthType length);

	/**
	 * Creates a new BytevectorCell from a copy of the passed data
	 *
	 * @param  world   World to create the bytevector cell in
	 * @param  data    Byte data to copy in to the new bytevector
	 * @param  length  Length of the data in bytes
	 */
	static BytevectorCell* fromData(World &world, const std::uint8_t *data, LengthType length);

	static const std::int16_t InvalidByte = -1;

	/**
	 * Creates a new bytevector with the given fill byte
	 *
	 * If a the required memory cannot be allocated then nullptr is returned
	 */
	static BytevectorCell* fromFill(World &world, LengthType length, std::uint8_t fill = 0);

	static BytevectorCell* fromAppended(World &world, const std::list<const BytevectorCell*> &byteVectors);

	bool operator==(const BytevectorCell &other) const;

	bool operator!=(const BytevectorCell &other) const
	{
		return !(*this == other);
	}

	BytevectorCell* copy(World &world, SliceIndexType start = 0, SliceIndexType end = -1) const;
	bool replace(LengthType offset, const BytevectorCell *from, SliceIndexType fromStart = 0, SliceIndexType fromEnd = -1);

	StringCell* utf8ToString(World &world, SliceIndexType start = 0, SliceIndexType end = -1);

	std::int16_t byteAt(LengthType offset) const
	{
		if (offset >= length())
		{
			return -1;
		}

		return byteArray()->data()[offset];
	}

	bool setByteAt(LengthType offset, std::uint8_t value)
	{
		assert(!isGlobalConstant());

		if (offset >= length())
		{
			return false;
		}

		// Break any sharing
		m_byteArray = m_byteArray->asWritable(length());

		byteArray()->data()[offset] = value;

		return true;
	}

	void finalizeBytevector();
};

}

#endif

