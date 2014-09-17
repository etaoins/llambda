#ifndef _LLIBY_DYNAMIC_STATE_H
#define _LLIBY_DYNAMIC_STATE_H

#include "binding/AnyCell.h"
#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/DynamicStateCell.h"

#include "alloc/cellref.h"

#include <unordered_map>

namespace lliby
{
namespace dynamic
{

class ParameterProcedureCell;

/**
 * Represents a dynamic state created by (parameterize) or (dynamic-wind)
 *
 * This can be viewed as a parallel stack the the program's call stack. Arbitrary values can be attached to the state
 * with (make-parameter) and (parameterize) and entry and exit procedures can be defined with (dynamic-wind)
 */
class State
{
public:
	typedef std::unordered_map<ParameterProcedureCell*, AnyCell*> ParameterValueMap;
	
	/**
	 * Creates a new state with a specified parent and before/after procedures
	 *
	 * @param  before      Procedure to invoke before activating this state or its children. nullptr will disable this 
	 *                     functionality.
	 * @param  after       Procedure to invoke after deactivating this state or children. nullptr will disable this 
	 *                     functionality.
	 * @param  parentCell  Cell pointer to the parent state or nullptr if this is a root state.
	 */
	State(TopProcedureCell *before, TopProcedureCell *after, DynamicStateCell *parentCell = nullptr);

	/**
	 * Returns the value for the passed parameter
	 *
	 * This will recurse up this state's ancestors. If no value is found the parameter's initial value will be returned.
	 */
	AnyCell *valueForParameter(ParameterProcedureCell *param) const;
	
	/**
	 * Sets the value for the passed parameter
	 *
	 * This only sets the value for this State instance; ancestor states are unmodified
	 */
	void setValueForParameter(World &world, ParameterProcedureCell *param, AnyCell *value);

	/**
	 * Applies a converter procedure to a parameter value
	 */
	static AnyCell* applyConverterProcedure(World &world, alloc::StrongRef<TopProcedureCell> &converterProc, AnyCell *value);

	/**
	 * Returns the parent cell for this state or nullptr if this is a root state
	 */
	DynamicStateCell *parentCell() const
	{
		return mParentCell;
	}

	/**
	 * Returns a pointer to our parent state or nullptr if this is the root state
	 */
	State *parent() const
	{
		if (mParentCell != nullptr)
		{
			return mParentCell->state();
		}

		return nullptr;
	}
	
	/**
	 * Returns a pointer to the parent cell
	 *
	 * This is used by the garbage collector to replace our parent pointer
	 */
	DynamicStateCell** parentCellRef()
	{
		return &mParentCell;
	}

	/**
	 * Returns the procedure to invoke before activating this state or its children
	 */
	TopProcedureCell *beforeProcedure()
	{
		return mBefore;
	}

	/**
	 * Returns a pointer to the before procedure cell
	 *
	 * This is intended for use by the garbage collector
	 */
	TopProcedureCell** beforeProcedureRef()
	{
		return &mBefore;
	}
	
	/**
	 * Returns the procedure to invoke after deactivating this state or its children
	 */
	TopProcedureCell *afterProcedure()
	{
		return mAfter;
	}
	
	/**
	 * Returns a pointer to the after procedure cell
	 *
	 * This is intended for use by the garbage collector
	 */
	TopProcedureCell** afterProcedureRef()
	{
		return &mAfter;
	}

	/**
	 * Returns the values of this state, excluding any ancestor states
	 *
	 * This is intended for use by the garbage collector
	 */
	const ParameterValueMap& selfValues() const
	{
		return mSelfValues;
	}

	/**
	 * Sets the parameter values of this state 
	 *
	 * This is intended for use by the garbage collector
	 */
	void setSelfValues(const ParameterValueMap &newValues)
	{
		mSelfValues = newValues;
	}	

	/**
	 * Returns the currently active state for this world
	 */
	static State* activeState(World &);

	/**
	 * Creates a child active of the currently active state and makes it active
	 *
	 * @param  world   World the state is being pushed in to
	 * @param  before  Procedure to invoke before activating this state or its children. nullptr will disable this 
	 *                 functionality.
	 * @param  after   Procedure to invoke after deactivating this state or children. nullptr will disable this 
	 *                 functionality.
	 *
	 * This may re-enter Scheme and invoke the garbage collector if before is not null
	 */
	static void pushActiveState(World &world, TopProcedureCell *before, TopProcedureCell *after);

	/**
	 * Makes the parent of the currently active state active
	 * 
	 * @param  world   World the state is being popped from
	 *
	 * This may re-enter Scheme and invoke the garbage collector
	 */
	static void popActiveState(World &world);

	/**
	 * Pops all active states
	 * 
	 * @param  world   World the states are being popped from
	 *
	 * This should be used when cleanly exiting from the world
	 */
	static void popAllStates(World &world);

	/**
	 * Switches to the specified state cell
	 *
	 * All "after" procedures for exiting states will be called followed by all "before" procedures for entering states.
	 */
	static void switchStateCell(World &world, DynamicStateCell *);

private:
	TopProcedureCell *mBefore;
	TopProcedureCell *mAfter;
	DynamicStateCell *mParentCell;
	ParameterValueMap mSelfValues;
};

}
}

#endif

