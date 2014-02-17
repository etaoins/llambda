#include "binding/ProcedureCell.h"
#include "binding/DatumCell.h"
#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"

#include "core/error.h"

using namespace lliby;
using lliby::dynamic::ParameterProcedureCell;

extern "C"
{

void _lliby_dynamicenv_push(World *world)
{
	dynamic::State::pushActiveState(world, nullptr, nullptr);
}

void _lliby_dynamicenv_set_value(World *, ProcedureCell *procCell, DatumCell *value)
{
	auto paramCell = datum_cast<ParameterProcedureCell>(procCell);

	if (paramCell == nullptr)
	{
		signalError("Attempted to parameterize non-parameter", {procCell});
	}

	dynamic::State::activeState()->setValueForParameter(paramCell, value);
}

void _lliby_dynamicenv_pop(World *world)
{
	dynamic::State::popActiveState(world);
}

}
