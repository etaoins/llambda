#include "binding/ProcedureCell.h"
#include "binding/AnyCell.h"
#include "binding/TypedProcedureCell.h"

#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"

#include "core/error.h"

using namespace lliby;
using lliby::dynamic::ParameterProcedureCell;

extern "C"
{

void llcore_dynamicenv_push(World &world)
{
	dynamic::State::pushActiveState(world);
}

void llcore_dynamicenv_set_value(World &world, ProcedureCell *procCell, AnyCell *value)
{
	auto paramCell = cell_cast<ParameterProcedureCell>(procCell);

	if (paramCell == nullptr)
	{
		signalError(world, ErrorCategory::InvalidArgument, "Attempted to parameterize non-parameter", {procCell});
	}

	dynamic::State::activeState(world)->setValueForParameter(world, paramCell, value);
}

void llcore_dynamicenv_pop(World &world)
{
	dynamic::State::popActiveState(world);
}

ProcedureCell *llcore_make_parameter(World &world, AnyCell *initialValue)
{
	return dynamic::ParameterProcedureCell::createInstance(world, initialValue);
}

AnyCell *llcore_value_for_parameter(World &world, ParameterProcedureCell *parameterProc)
{
	return dynamic::State::activeState(world)->valueForParameter(parameterProc);
}

}
