#ifndef _LLIBY_BINDING_PROCEDURECELL_H
#define _LLIBY_BINDING_PROCEDURECELL_H

#include "RecordLikeCell.h"

namespace lliby
{
class World;

/**
 * Represents a Scheme procedure of an unknown type
 *
 * Untyped procedures can only have garbage collection performed on them. To be applied their exact type must be known.
 * See TypedProcedureCell for a subclass that that supports application.
 */
class ProcedureCell : public RecordLikeCell
{
#include "generated/ProcedureCellMembers.h"
public:
	ProcedureCell(RecordClassIdType recordClassId, bool dataIsInline, void *recordData, void *entryPoint) :
		RecordLikeCell(CellTypeId::Procedure, recordClassId, dataIsInline, recordData),
		m_entryPoint(entryPoint)
	{
	}

	static ProcedureCell* createInstance(World &World, RecordClassIdType recordClassId, bool dataIsInline, void *recordData, void *entryPoint);

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

