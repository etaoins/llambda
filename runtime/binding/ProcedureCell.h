#ifndef _LLIBY_BINDING_PROCEDURECELL_H
#define _LLIBY_BINDING_PROCEDURECELL_H

#include "RecordLikeCell.h"
#include "core/World.h"

namespace lliby
{

class ProcedureCell : public RecordLikeCell
{
#include "generated/ProcedureCellMembers.h"
public:
	ProcedureCell(std::uint32_t recordClassId, bool dataIsInline, void *recordData, ProcedureEntryPoint entryPoint) :
		RecordLikeCell(CellTypeId::Procedure, recordClassId, dataIsInline, recordData),
		m_entryPoint(entryPoint)
	{
	}

	DatumCell* apply(World *world, ListElementCell *arguments);

	/**
	 * Indicates if this procedure captures variables from its enclosing scope
	 */
	bool capturesVariables() const
	{
		return recordClassId() != EmptyClosureRecordClassId;
	}
};

}

#endif

