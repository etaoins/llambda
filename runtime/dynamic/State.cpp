#include "dynamic/State.h"

#include "core/World.h"
#include "core/error.h"
#include "dynamic/ParameterProcedureCell.h"
#include "alloc/allocator.h"
#include "binding/EmptyListCell.h"
#include "binding/ProperList.h"

namespace lliby
{
namespace dynamic
{

State::State(State *parent) :
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
	m_selfValues[param] = value;
}

State* State::activeState(World &world)
{
	return world.activeState();
}

void State::pushActiveState(World &world)
{
	// Create a state and associate it with the dynamic state
	world.setActiveState(new State(world.activeState()));
}

void State::popActiveState(World &world)
{
	State *oldActiveState = world.activeState();

	// Make the old state active and reference it
	world.setActiveState(oldActiveState->parent());
	delete oldActiveState;
}

void State::popUntilState(World &world, State *targetState)
{
	while(world.activeState() != targetState)
	{
		popActiveState(world);
	}

}

}
}
