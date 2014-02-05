#include "core/dynamic.h"

#include "binding/ProcedureCell.h"
#include "binding/DatumCell.h"
#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"

#include "core/error.h"

using namespace lliby;
using lliby::dynamic::ParameterProcedureCell;

extern "C"
{

void _lliby_dynamicenv_push()
{
	dynamic::State::pushActiveState(nullptr, nullptr);
}

void _lliby_dynamicenv_set_value(ProcedureCell *procCell, DatumCell *value)
{
	auto paramCell = datum_cast<ParameterProcedureCell>(procCell);

	if (paramCell == nullptr)
	{
		signalError("Attempted to parameterize non-parameter", {procCell});
	}

	dynamic::State::activeState()->setValueForParameter(paramCell, value);
}

void _lliby_dynamicenv_pop()
{
	dynamic::State::popActiveState();
}

}
