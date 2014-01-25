#include "dynamic/State.h"

#include "dynamic/ParameterProcedureCell.h"
#include "alloc/StrongRef.h"
#include "binding/EmptyListCell.h"

namespace lliby
{
namespace dynamic
{

namespace
{
	State *currentActiveState = new State(nullptr, nullptr);
}
	
State::State(ProcedureCell *before, ProcedureCell *after, State *parent) : 
	mBefore(before),
	mAfter(after),
	mParent(parent)
{
}
	
DatumCell* State::valueForParameter(ParameterProcedureCell *param) const
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

void State::setValueForParameter(ParameterProcedureCell *param, DatumCell *value)
{
	mSelfValues[param] = value;
}

State* State::activeState()
{
	return currentActiveState;
}

void State::pushActiveState(ProcedureCell *before, ProcedureCell *after)
{
	if (before != nullptr)
	{
		// Avoid rooting these if we don't have to
		alloc::StrongRefRange<ProcedureCell> beforeRoot(&before, 1);
		alloc::StrongRefRange<ProcedureCell> afterRoot(&after, 1);

		// Invoke the before procedure 
		before->apply(EmptyListCell::instance());
	}
	
	currentActiveState = new State(before, after, currentActiveState);
}

void State::popActiveState()
{
	State *oldActiveState = currentActiveState;

	// Make the old state active and reference it
	currentActiveState = currentActiveState->parent();

	delete oldActiveState;

	if (oldActiveState->afterProcedure())
	{
		oldActiveState->afterProcedure()->apply(EmptyListCell::instance());
	}
}

}
}
