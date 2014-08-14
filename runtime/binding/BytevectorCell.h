#ifndef _LLIBY_BINDING_BYTEVECTORCELL_H
#define _LLIBY_BINDING_BYTEVECTORCELL_H

#include "AnyCell.h"
#include <list>
#include <cassert>

namespace lliby
{

class BytevectorCell : public AnyCell
{
#include "generated/BytevectorCellMembers.h"
public:
	/**
	 * Creates a new BytevectorCell backed by the passed SharedByteArray
	 *
	 * The SharedByteArray must already have a reference taken for the bytevector cell
	 *
	 * @param  world      World to create the bytevector cell in
	 * @param  byteArray  Byte array to back the bytevector cell
	 * @param  length     Length of the data in bytes
	 */
	static BytevectorCell* withByteArray(World &world, SharedByteArray *byteArray, std::uint32_t length);
	
	/**
	 * Creates a new BytevectorCell from a copy of the passed data
	 *
	 * @param  world   World to create the bytevector cell in
	 * @param  data    Byte data to copy in to the new bytevector
	 * @param  length  Length of the data in bytes
	 */
	static BytevectorCell* fromData(World &world, const std::uint8_t *data, std::uint32_t length);

	static const std::int16_t InvalidByte = -1;

	static BytevectorCell* fromFill(World &world, std::uint32_t length, std::uint8_t fill = 0);
	static BytevectorCell* fromAppended(World &world, const std::list<const BytevectorCell*> &byteVectors);

	BytevectorCell* copy(World &world, std::int64_t start = 0, std::int64_t end = -1) const; 
	bool replace(std::uint32_t offset, const BytevectorCell *from, std::int64_t fromStart = 0, std::int64_t fromEnd = -1);

	StringCell* utf8ToString(World &world, std::int64_t start = 0, std::int64_t end = -1);

	std::int16_t byteAt(std::uint32_t offset) const
	{
		if (offset >= length())
		{
			return -1;
		}
		
		return byteArray()->data()[offset];
	}

	bool setByteAt(std::uint32_t offset, std::uint8_t value)
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

protected:
	BytevectorCell(SharedByteArray *byteArray, std::uint32_t length) :
		AnyCell(CellTypeId::Bytevector),
		m_length(length),
		m_byteArray(byteArray)
	{
	}
};

}

#endif

