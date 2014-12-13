#include "dynamic/State.h"

#include "core/World.h"
#include "core/error.h"
#include "dynamic/ParameterProcedureCell.h"
#include "alloc/cellref.h"
#include "alloc/allocator.h"
#include "binding/EmptyListCell.h"
#include "binding/DynamicStateCell.h"
#include "binding/ProperList.h"

namespace lliby
{
namespace dynamic
{

namespace
{
	/**
	 * Returns the path of states from "descendant" to "ancestor" in descendant to ancestor order
	 *
	 * If "descendant" isn't a descendant of "ancestor" an empty list is returned
	 */
	std::vector<DynamicStateCell*> pathToAncestorState(DynamicStateCell *descendant, DynamicStateCell *ancestor)
	{
		std::vector<DynamicStateCell*> path;

		while(descendant != nullptr)
		{
			if (descendant == ancestor)
			{
				return path;
			}

			path.push_back(descendant);
			descendant = descendant->state()->parentCell();
		}

		// Fell off the end of the list; we're not an ancestor
		return std::vector<DynamicStateCell*>();
	}
}
	
State::State(ThunkProcedureCell *before, ThunkProcedureCell *after, DynamicStateCell *parentCell) : 
	mBefore(before),
	mAfter(after),
	mParentCell(parentCell)
{
}
	
AnyCell* State::valueForParameter(ParameterProcedureCell *param) const
{
	ParameterValueMap::const_iterator valueIt = mSelfValues.find(param);

	if (valueIt != mSelfValues.end())
	{
		return valueIt->second;
	}
	else if (mParentCell)
	{
		return mParentCell->state()->valueForParameter(param);
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

	mSelfValues[param] = value;
}

State* State::activeState(World &world)
{
	return world.activeStateCell->state();
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
	
	// Allocate the dynamic state cell
	DynamicStateCell *pushedDynamicStateCell = DynamicStateCell::createInstance(world, nullptr);
	
	// Create a state and associated it with the dynamic state cell
	auto pushedState = new State(beforeRef, afterRef, world.activeStateCell);
	pushedDynamicStateCell->setState(pushedState);

	world.activeStateCell = pushedDynamicStateCell;
}

void State::popActiveState(World &world)
{
	State *oldActiveState = world.activeStateCell->state();

	// Make the old state active and reference it
	world.activeStateCell = world.activeStateCell->state()->parentCell();

	// After is executed in the "outer" state
	if (oldActiveState->afterProcedure())
	{
		oldActiveState->afterProcedure()->apply(world);
	}
}
	
void State::popAllStates(World &world)
{
	while(world.activeStateCell->state()->parentCell() != nullptr)
	{
		popActiveState(world);
	}
}
	
void State::switchStateCell(World &world, DynamicStateCell *targetStateCell)
{
	alloc::DynamicStateRef targetStateRef(world, targetStateCell);

#ifdef _LLIBY_ALWAYS_GC
	// We can potentially GC if we cross a state that has a before/after procedure that allocates memory. However, this
	// isn't very common. Ensure people are properly rooting their values by forcing a collection on every state switch.
	alloc::forceCollection(world);
#endif

	while(true)
	{
		if (world.activeStateCell == targetStateRef.data())
		{
			// We're done
			return;
		}

		// Is the target state a descendant of our current state? 
		std::vector<DynamicStateCell*> pathToTargetState = pathToAncestorState(targetStateRef, world.activeStateCell);

		if (!pathToTargetState.empty())
		{
			// Root the path in case the before procedures cause GC
			alloc::StrongRoot<DynamicStateCell> pathRef(world, pathToTargetState.data(), pathToTargetState.size());

			for(auto it = pathToTargetState.rbegin(); it != pathToTargetState.rend(); it++)
			{
				if ((*it)->state()->beforeProcedure())
				{
					(*it)->state()->beforeProcedure()->apply(world);
				}

				world.activeStateCell = *it; 
			}

			// We're done
			return;
		}

		// No, climb the state tree more
		popActiveState(world);
	}
}

}
}
