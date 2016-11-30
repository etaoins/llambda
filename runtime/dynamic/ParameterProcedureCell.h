#ifndef _LLIBY_DYNAMIC_PARAMETERPROCEDURECELL_H
#define _LLIBY_DYNAMIC_PARAMETERPROCEDURECELL_H

#include "binding/TypedProcedureCell.h"

namespace lliby
{

class World;

namespace dynamic
{

class ParameterProcedureCell : public TopProcedureCell
{
public:
	/**
	 * Creates a new parameter procedure cell instance
	 *
	 * This will enter the allocator and can potentially trigger GC
	 *
	 * @param initialValue        Initial value for the parameter
	 */
	ParameterProcedureCell(AnyCell *initialValue);

	/**
	 * Creates a new parameter procedure cell instance in the passed world
	 */
	static ParameterProcedureCell *createInstance(World &world, AnyCell *initialValue);

	/**
	 * Returns the initial value for this parameter
	 *
	 * This is used whenever the parameter hasn't explicitly been parameterized
	 */
	AnyCell* initialValue() const
	{
		return static_cast<AnyCell*>(recordData());
	}

	/**
	 * Returns true if the passed cell is a ParameterProcedureCell
	 */
	static bool isInstance(const AnyCell *cell)
	{
		auto procedureCell = cell_cast<ProcedureCell>(cell);
		return procedureCell && isInstance(procedureCell);
	}

	/**
	 * Returns true if the passed procedure cell is a ParameterProcedureCell
	 */
	static bool isInstance(const ProcedureCell *);

	/**
	 * Registers the record class for the parameter procedure's closure
	 *
	 * This is called by dynamic::init() at startup; this should not be directly invoked
	 */
	static void registerRecordClass();
};

}
}

#endif

