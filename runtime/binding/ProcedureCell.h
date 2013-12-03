#ifndef _LLIBY_BINDING_PROCEDURECELL_H
#define _LLIBY_BINDING_PROCEDURECELL_H

#include "RecordLikeCell.h"

namespace lliby
{

class ProcedureCell : public RecordLikeCell
{
#include "generated/ProcedureCellMembers.h"
public:
	ProcedureCell(std::uint32_t recordClassId, void *recordData, ProcedureEntryPoint entryPoint) :
		RecordLikeCell(CellTypeId::Procedure, recordClassId, recordData),
		m_entryPoint(entryPoint)
	{
	}

	DatumCell* invoke(ListElementCell *arguments);
};

}

#endif

