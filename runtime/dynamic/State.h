#ifndef _LLIBY_DYNAMIC_STATE_H
#define _LLIBY_DYNAMIC_STATE_H

#include "binding/DatumCell.h"
#include "binding/ProcedureCell.h"

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
	typedef std::unordered_map<ParameterProcedureCell*, DatumCell*> ParameterValueMap;
	
	/**
	 * Creates a new state with a specified parent and before/after procedures
	 *
	 * @param  before  Procedure to invoke before activating this state or its children. nullptr will disable this 
	 *                 functionality.
	 * @param  after   Procedure to invoke after deactivating this state or children. nullptr will disable this 
	 *                 functionality.
	 * @param  parent  Parent of the state or nullptr if this is a root state. The parent will have its reference count
	 *                 incremented.
	 */
	State(ProcedureCell *before, ProcedureCell *after, State *parent = nullptr);

	/**
	 * Returns the value for the passed parameter
	 *
	 * This will recurse up this state's ancestors. If no value is found the parameter's initial value will be returned.
	 */
	DatumCell *valueForParameter(ParameterProcedureCell *param) const;
	
	/**
	 * Sets the value for the passed parameter
	 *
	 * This only sets the value for this State instance; ancestor states are unmodified
	 */
	void setValueForParameter(ParameterProcedureCell *param, DatumCell *value);

	/**
	 * Returns the parent for this state or nullptr if this is a root state
	 */
	State *parent() const
	{
		return mParent;
	}

	/**
	 * Returns the procedure to invoke before activating this state or its children
	 */
	ProcedureCell *beforeProcedure()
	{
		return mBefore;
	}

	/**
	 * Returns a pointer to the before procedure cell
	 *
	 * This is intended for use by the garbage collector
	 */
	ProcedureCell** beforeProcedureRef()
	{
		return &mBefore;
	}
	
	/**
	 * Returns the procedure to invoke after deactivating this state or its children
	 */
	ProcedureCell *afterProcedure()
	{
		return mAfter;
	}
	
	/**
	 * Returns a pointer to the after procedure cell
	 *
	 * This is intended for use by the garbage collector
	 */
	ProcedureCell** afterProcedureRef()
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
	static void pushActiveState(World &world, ProcedureCell *before, ProcedureCell *after);

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
	 * Switches to the specified state
	 *
	 * Without continuations this will just pop until the state matches the passed argument
	 */
	static void switchState(World &world, State *);

private:
	ProcedureCell *mBefore;
	ProcedureCell *mAfter;
	State *mParent;
	ParameterValueMap mSelfValues;
};

}
}

#endif

