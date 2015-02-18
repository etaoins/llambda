#ifndef _LLIBY_DYNAMIC_PARAMETERPROCEDURECELL_H
#define _LLIBY_DYNAMIC_PARAMETERPROCEDURECELL_H

#include "binding/TypedProcedureCell.h"
#include "dynamic/ConverterProcedureCell.h"

namespace lliby
{

class World;

namespace dynamic
{

class ParameterProcedureCell : public TopProcedureCell
{
private:
	struct ParameterProcedureClosure
	{
		AnyCell *initialValue;
		// If there is no converter it will be an instance of UnspecificCell
		// This is to prevent special casing elsewhere in the runtime
		AnyCell *converter;
	};

public:
	/**
	 * Creates a new parameter procedure cell instance
	 *
	 * This will enter the allocator and can potentially trigger GC
	 *
	 * @param world               Current world pointer
	 * @param initialValue        Initial value for the parameter. This should be pre-converted using
	 *                            converterProcedure by the caller. State::applyConverterProcedure() can be used for
	 *                            this purpose.
	 * @param converterProcedure  Procedure to invoke to convert parameterized values for this parameter. If the
	 *                            identity function should be used nullptr can be passed to avoid the overhead of
	 *                            re-entering Scheme,
	 */
	ParameterProcedureCell(AnyCell *initialValue, ConverterProcedureCell *converterProcedure = nullptr);

	/**
	 * Creates a new parameter procedure cell instance in the passed world
	 */
	static ParameterProcedureCell *createInstance(World &world, AnyCell *initialValue, ConverterProcedureCell *converterProcedure = nullptr);

	/**
	 * Returns the initial value for this parameter
	 *
	 * This is used whenever the parameter hasn't explicitly been parameterized
	 */
	AnyCell* initialValue() const
	{
		return static_cast<ParameterProcedureClosure*>(recordData())->initialValue;
	}
	
	/**
	 * Returns the converter procedure for this parameter or null if the identity function should be used
	 */
	ConverterProcedureCell* converterProcedure() const
	{
		auto procCell = cell_cast<ProcedureCell>(static_cast<ParameterProcedureClosure*>(recordData())->converter);
		return static_cast<ConverterProcedureCell*>(procCell);
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

