#include "binding/ProcedureCell.h"
#include "binding/AnyCell.h"
#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"

#include "core/error.h"

using namespace lliby;
using lliby::dynamic::ParameterProcedureCell;

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

	dynamic::State::activeState(world)->setValueForParameter(paramCell, value);
}

void _lliby_dynamicenv_pop(World &world)
{
	dynamic::State::popActiveState(world);
}

}
