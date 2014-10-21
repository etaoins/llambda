#ifndef _LLIBY_BINDING_VECTORCELL_H
#define _LLIBY_BINDING_VECTORCELL_H

#include "AnyCell.h"
#include <vector>
#include <cassert>

namespace lliby
{

class VectorCell : public AnyCell
{
#include "generated/VectorCellMembers.h"
public:
	AnyCell* elementAt(std::uint32_t offset) const
	{
		if (offset >= length())
		{
			return nullptr;
		}
		
		return elements()[offset];
	}

	bool setElementAt(std::uint32_t offset, AnyCell *value)
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
	static VectorCell* fromElements(World &world, AnyCell **elements, std::uint32_t length);

	static VectorCell* fromFill(World &world, std::uint32_t length, AnyCell *fill = nullptr);
	static VectorCell* fromAppended(World &world, const std::vector<const VectorCell*> &vectors);
	
	VectorCell* copy(World &world, std::int64_t start = 0, std::int64_t end = -1); 
	bool replace(std::uint32_t offset, const VectorCell *from, std::int64_t fromStart = 0, std::int64_t fromEnd = -1);

	bool fill(AnyCell *fill, std::int64_t start = 0, std::int64_t end = -1);
	
	void finalizeVector();

protected:
	VectorCell(AnyCell **elements, std::uint32_t length) :
		AnyCell(CellTypeId::Vector),
		m_length(length),
		m_elements(elements)
	{
	}
};

}


#endif
