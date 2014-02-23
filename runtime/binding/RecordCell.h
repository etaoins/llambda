#ifndef _LLIBY_BINDING_RECORDCELL_H
#define _LLIBY_BINDING_RECORDCELL_H

#include "RecordLikeCell.h"

namespace lliby
{
class World;

class RecordCell : public RecordLikeCell
{
#include "generated/RecordCellMembers.h"
public:
	static RecordCell* createInstance(World &world, std::uint32_t recordClassId, bool dataIsInline, void *recordData);

protected:
	RecordCell(std::uint32_t recordClassId, bool dataIsInline, void *recordData) :
		RecordLikeCell(CellTypeId::Record, recordClassId, dataIsInline, recordData)
	{
	}
};

}

#endif

