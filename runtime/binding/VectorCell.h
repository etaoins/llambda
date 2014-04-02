#ifndef _LLIBY_BINDING_VECTORCELL_H
#define _LLIBY_BINDING_VECTORCELL_H

#include "DatumCell.h"
#include <vector>
#include <cassert>

namespace lliby
{

class VectorCell : public DatumCell
{
#include "generated/VectorCellMembers.h"
public:
	DatumCell* elementAt(std::uint32_t offset) const
	{
		if (offset >= length())
		{
			return nullptr;
		}
		
		return elements()[offset];
	}

	bool setElementAt(std::uint32_t offset, DatumCell *value)
	{
		if (offset >= length())
		{
			return false;
		}

		assert(!isGlobalConstant());
		elements()[offset] = value;

		return true;
	}

	static VectorCell* fromElements(World &world, DatumCell **elements, std::uint32_t length);
	static VectorCell* fromFill(World &world, std::uint32_t length, DatumCell *fill = nullptr);
	static VectorCell* fromAppended(World &world, const std::vector<const VectorCell*> &vectors);
	
	VectorCell* copy(World &world, std::int64_t start = 0, std::int64_t end = -1); 
	bool replace(std::uint32_t offset, const VectorCell *from, std::int64_t fromStart = 0, std::int64_t fromEnd = -1);

	bool fill(DatumCell *fill, std::int64_t start = 0, std::int64_t end = -1);
	
	void finalizeVector();

protected:
	VectorCell(DatumCell **elements, std::uint32_t length) :
		DatumCell(CellTypeId::Vector),
		m_length(length),
		m_elements(elements)
	{
	}
};

}


#endif
