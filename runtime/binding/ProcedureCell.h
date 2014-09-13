#ifndef _LLIBY_BINDING_PROCEDURECELL_H
#define _LLIBY_BINDING_PROCEDURECELL_H

#include "RecordLikeCell.h"
#include "ReturnValuesList.h"

namespace lliby
{
class World;

class ProcedureCell : public RecordLikeCell
{
#include "generated/ProcedureCellMembers.h"
public:
	static ProcedureCell* createInstance(World &World, std::uint32_t recordClassId, bool dataIsInline, void *recordData, ProcedureEntryPoint entryPoint);

	/**
	 * Applies this procedure
	 *
	 * @param  world      Active world object
	 * @param  arguments  Proper list of arguments to pass to the procedure
	 * @return  Multiple return value list of results
	 */
	ReturnValuesList* apply(World &world, ListElementCell *arguments);

	/**
	 * Indicates if this procedure captures variables from its enclosing scope
	 */
	bool capturesVariables() const
	{
		return recordClassId() != EmptyClosureRecordClassId;
	}

protected:
	ProcedureCell(std::uint32_t recordClassId, bool dataIsInline, void *recordData, ProcedureEntryPoint entryPoint) :
		RecordLikeCell(CellTypeId::Procedure, recordClassId, dataIsInline, recordData),
		m_entryPoint(entryPoint)
	{
	}
};

}

#endif

