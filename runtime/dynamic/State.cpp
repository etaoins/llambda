#include "dynamic/State.h"

#include "core/World.h"
#include "core/error.h"
#include "dynamic/ParameterProcedureCell.h"
#include "alloc/cellref.h"
#include "alloc/allocator.h"
#include "binding/EmptyListCell.h"
#include "binding/ProperList.h"

namespace lliby
{
namespace dynamic
{

State::State(ThunkProcedureCell *before, ThunkProcedureCell *after, State *parent) :
	m_before(before),
	m_after(after),
	m_parent(parent)
{
}

AnyCell* State::valueForParameter(ParameterProcedureCell *param) const
{
	ParameterValueMap::const_iterator valueIt = m_selfValues.find(param);

	if (valueIt != m_selfValues.end())
	{
		return valueIt->second;
	}
	else if (m_parent)
	{
		return m_parent->valueForParameter(param);
	}
	else
	{
		return param->initialValue();
	}
}

void State::setValueForParameter(World &world, ParameterProcedureCell *param, AnyCell *value)
{
	ConverterProcedureCell *converterProcRaw = param->converterProcedure();

	if (converterProcRaw)
	{
		alloc::StrongRoot<ParameterProcedureCell> paramRoot(world, &param);
		alloc::StrongRef<ConverterProcedureCell> converterProc(world, converterProcRaw);

		value = converterProc->apply(world, value);
	}

	m_selfValues[param] = value;
}

State* State::activeState(World &world)
{
	return world.activeState();
}

void State::pushActiveState(World &world, ThunkProcedureCell *before, ThunkProcedureCell *after)
{
	alloc::StrongRef<ThunkProcedureCell> beforeRef(world, before);
	alloc::StrongRef<ThunkProcedureCell> afterRef(world, after);

	// Invoke the before procedure
	if (beforeRef)
	{
		beforeRef->apply(world);
	}

	// Create a state and associated it with the dynamic state
	world.setActiveState(new State(beforeRef, afterRef, world.activeState()));
}

void State::popActiveState(World &world)
{
	State *oldActiveState = world.activeState();

	// Make the old state active and reference it
	world.setActiveState(oldActiveState->parent());

	// After is executed in the "outer" state
	if (oldActiveState->afterProcedure())
	{
		oldActiveState->afterProcedure()->apply(world);
	}

	delete oldActiveState;
}

void State::popAllStates(World &world)
{
	while(world.activeState()->parent() != nullptr)
	{
		popActiveState(world);
	}
}

void State::popUntilState(World &world, State *targetState)
{
#ifdef _LLIBY_ALWAYS_GC
	// We can potentially GC if we cross a state that has a before/after procedure that allocates memory. However, this
	// isn't very common. Ensure people are properly rooting their values by forcing a collection on every state switch.
	alloc::forceCollection(world);
#endif

	while(world.activeState() != targetState)
	{
		popActiveState(world);
	}

}

}
}
