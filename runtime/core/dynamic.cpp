#include "core/dynamic.h"

#include "core/fatal.h"

#include "binding/ProcedureCell.h"
#include "binding/DatumCell.h"
#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"

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
	if (!ParameterProcedureCell::isInstance(procCell))
	{
		_lliby_fatal("Attempted to parameterize non-parameter", procCell);
	}

	auto paramCell = static_cast<ParameterProcedureCell*>(procCell);
	dynamic::State::activeState()->setValueForParameter(paramCell, value);
}

void _lliby_dynamicenv_pop()
{
	dynamic::State::popActiveState();
}

}
