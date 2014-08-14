#include "dynamic/State.h"

#include "core/World.h"
#include "dynamic/ParameterProcedureCell.h"
#include "alloc/cellref.h"
#include "binding/EmptyListCell.h"

namespace lliby
{
namespace dynamic
{
	
State::State(ProcedureCell *before, ProcedureCell *after, State *parent) : 
	mBefore(before),
	mAfter(after),
	mParent(parent)
{
}
	
AnyCell* State::valueForParameter(ParameterProcedureCell *param) const
{
	ParameterValueMap::const_iterator valueIt = mSelfValues.find(param);

	if (valueIt != mSelfValues.end())
	{
		return valueIt->second;
	}
	else if (mParent)
	{
		return mParent->valueForParameter(param);
	}
	else
	{
		return param->initialValue();
	}
}

void State::setValueForParameter(ParameterProcedureCell *param, AnyCell *value)
{
	mSelfValues[param] = value;
}

State* State::activeState(World &world)
{
	return world.activeState;
}

void State::pushActiveState(World &world, ProcedureCell *before, ProcedureCell *after)
{
	if (before != nullptr)
	{
		// Avoid rooting these if we don't have to
		alloc::ProcedureRefRange beforeRoot(world, &before, 1);
		alloc::ProcedureRefRange afterRoot(world, &after, 1);

		// Invoke the before procedure 
		before->apply(world, EmptyListCell::instance());
	}
	
	world.activeState = new State(before, after, world.activeState);
}

void State::popActiveState(World &world)
{
	State *oldActiveState = world.activeState;

	// Make the old state active and reference it
	world.activeState = world.activeState->parent();

	if (oldActiveState->afterProcedure())
	{
		oldActiveState->afterProcedure()->apply(world, EmptyListCell::instance());
	}
	
	delete oldActiveState;
}
	
void State::popAllStates(World &world)
{
	while(world.activeState->parent() != nullptr)
	{
		popActiveState(world);
	}
}
	
void State::switchState(World &world, State *state)
{
	while(world.activeState != state)
	{
		popActiveState(world);
	}
}

}
}
