#ifndef _LLIBY_DYNAMIC_ESCAPEPROCEDURECELL_H
#define _LLIBY_DYNAMIC_ESCAPEPROCEDURECELL_H

#include "binding/ProcedureCell.h"
#include "alloc/StrongRef.h"
#include "alloc/cellref.h"

namespace lliby
{

class World;

namespace dynamic
{

class EscapeProcedureCell : public ProcedureCell
{
private:
	struct EscapeProcedureClosure
	{
		bool invalidated;
	};

public:
	/**
	 * Creates a new escape procedure cell instance
	 *
	 * This will enter the allocator and can potentially trigger GC
	 *
	 * @param world               Current world pointer
	 *                            re-entering Scheme,
	 */
	static EscapeProcedureCell *createInstance(World &world);
	
	/**
	 * Registers the record class for the escape procedure's closure
	 *
	 * This is called by dynamic::init() at startup; this should not be directly invoked
	 */
	static void registerRecordClass();

	/**
	 * Must be called once an escape procedure falls out of its scope
	 *
	 * This is a side effect of call/cc being (incompletely) emulated using exceptions
	 */
	void invalidate();

	/**
	 * Returns true if this instance has been invalidated
	 */
	bool isInvalidated() const;
};

class EscapeProcedureInvokedException
{
public:
	EscapeProcedureInvokedException(World &world, EscapeProcedureCell *escapeProcedure, DatumCell *passedValue) :
		m_escapeProcedureRef(world, escapeProcedure),
		m_passedValueRef(world, passedValue)
	{
	}

	EscapeProcedureCell *escapeProcedure() const
	{
		return m_escapeProcedureRef.data();
	}

	DatumCell *passedValue() const
	{
		return m_passedValueRef.data();
	}

private:
	alloc::StrongRef<EscapeProcedureCell> m_escapeProcedureRef;
	alloc::DatumRef m_passedValueRef;
};

}

}

#endif
