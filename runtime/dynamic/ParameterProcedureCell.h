#ifndef _LLIBY_DYNAMIC_PARAMETERPROCEDURECELL_H
#define _LLIBY_DYNAMIC_PARAMETERPROCEDURECELL_H

#include "binding/ProcedureCell.h"

namespace lliby
{
namespace dynamic
{

class ParameterProcedureCell : public ProcedureCell
{
private:
	struct ParameterProcedureClosure
	{
		DatumCell *initialValue;
		// If there is no converter it will be an instance of UnspecificCell
		// This is to prevent special casing elsewhere in the runtime
		DatumCell *converter;
	};

public:
	/**
	 * Creates a new procedure cell instance
	 *
	 * This will enter the allocator and can potentially trigger GC
	 *
	 * @param initialValue        Initial value for the parameter. This should be pre-convertered using
	 *                            converterProcedure by the caller.
	 *               
	 * @param converterProcedure  Procedure to invoke to convert parameterized values for this parameter. If the 
	 *                            identity function should be used nullptr can be passed to avoid the overhead of
	 *                            re-entering Scheme,
	 */
	ParameterProcedureCell(DatumCell *initialValue, ProcedureCell *converterProcedure = nullptr);
 
	/**
	 * Returns the initial value for this parameter
	 *
	 * This is used whenever the parameter hasn't explicitly been parameterized
	 */
	DatumCell* initialValue() const
	{
		return static_cast<ParameterProcedureClosure*>(recordData())->initialValue;
	}
	
	/**
	 * Returns the converter procedure for this parameter or null if the identity function should be used
	 */
	ProcedureCell* converterProcedure() const
	{
		return datum_cast<ProcedureCell>(static_cast<ParameterProcedureClosure*>(recordData())->converter);
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

