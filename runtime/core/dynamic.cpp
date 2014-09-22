#include "binding/ProcedureCell.h"
#include "binding/AnyCell.h"
#include "binding/TypedProcedureCell.h"

#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"
#include "dynamic/ConverterProcedureCell.h"

#include "core/error.h"

using namespace lliby;
using lliby::dynamic::ParameterProcedureCell;
using lliby::dynamic::ConverterProcedureCell;

extern "C"
{

void _lliby_dynamicenv_push(World &world)
{
	dynamic::State::pushActiveState(world, nullptr, nullptr);
}

void _lliby_dynamicenv_set_value(World &world, ProcedureCell *procCell, AnyCell *value)
{
	auto paramCell = cell_cast<ParameterProcedureCell>(procCell);

	if (paramCell == nullptr)
	{
		signalError(world, "Attempted to parameterize non-parameter", {procCell});
	}

	dynamic::State::activeState(world)->setValueForParameter(world, paramCell, value);
}

void _lliby_dynamicenv_pop(World &world)
{
	dynamic::State::popActiveState(world);
}

ProcedureCell *_lliby_make_parameter(World &world, AnyCell *initialValue, AnyCell *converterValue)
{
	// Scheme will pass in #!unit if it doesn't want a converter
	// This will become nullptr which is what C++ expects to disable conversion
	auto converterProcRaw = static_cast<ConverterProcedureCell*>(cell_cast<ProcedureCell>(converterValue));

	// Try to only GC root things if we have a converter - the majority of procedures don't
	if (converterProcRaw != nullptr)
	{
		// Convert initialValue
		alloc::StrongRef<ConverterProcedureCell> converterProc(world, converterProcRaw);

		initialValue = converterProc->apply(world, initialValue);
		converterProcRaw = converterProc.data();
	}

	return dynamic::ParameterProcedureCell::createInstance(world, initialValue, converterProcRaw);
}

AnyCell *_lliby_value_for_parameter(World &world, ParameterProcedureCell *parameterProc)
{
	return dynamic::State::activeState(world)->valueForParameter(parameterProc);
}

}
