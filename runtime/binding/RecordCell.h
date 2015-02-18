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
	RecordCell(RecordClassIdType recordClassId, bool dataIsInline, void *recordData) :
		RecordLikeCell(CellTypeId::Record, recordClassId, dataIsInline, recordData)
	{
	}

	RecordCell(RecordClassIdType recordClassId, bool dataIsInline, void *recordData, GarbageState gcState) :
		RecordLikeCell(CellTypeId::Record, recordClassId, dataIsInline, recordData, gcState)
	{
	}

	static RecordCell* createInstance(World &world, RecordClassIdType recordClassId, bool dataIsInline, void *recordData);
};

}

#endif

