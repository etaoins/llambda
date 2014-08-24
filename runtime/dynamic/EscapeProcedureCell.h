#ifndef _LLIBY_DYNAMIC_ESCAPEPROCEDURECELL_H
#define _LLIBY_DYNAMIC_ESCAPEPROCEDURECELL_H

#include "binding/ProcedureCell.h"
#include "alloc/StrongRef.h"
#include "alloc/cellref.h"
#include "alloc/CellRefRangeList.h"

namespace lliby
{

class World;

namespace dynamic
{

class Continuation;

class EscapeProcedureCell : public ProcedureCell
{
private:
	struct EscapeProcedureClosure
	{
	};

public:
	/**
	 * Creates a new escape procedure cell instance
	 *
	 * This will enter the allocator and can potentially trigger GC
	 *
	 * @param  world         Current world pointer
	 * @param  continuation  Continuation to resume for when this escape procedure is applied
	 */
	static EscapeProcedureCell *createInstance(World &world, Continuation *continuation);
	
	/**
	 * Registers the record class for the escape procedure's closure
	 *
	 * This is called by dynamic::init() at startup; this should not be directly invoked
	 */
	static void registerRecordClass();
	
	/**
	 * Returns true if the passed cell is a EscapeProcedureCell
	 */
	static bool isInstance(const AnyCell *cell)
	{
		auto procedureCell = cell_cast<ProcedureCell>(cell);
		return procedureCell && isInstance(procedureCell);
	}

	/**
	 * Returns true if the passed procedure cell is a EscapeProcedureCell
	 */
	static bool isInstance(const ProcedureCell *);

	/**
	 * Returns the continuation associated with this escape procedure
	 */
	Continuation *continuation() const;

	/**
	 * Sets the continuation for this escape procedure
	 */
	void setContinuation(Continuation *);
};

}

}

#endif
