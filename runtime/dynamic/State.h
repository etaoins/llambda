#ifndef _LLIBY_DYNAMIC_STATE_H
#define _LLIBY_DYNAMIC_STATE_H

#include "binding/AnyCell.h"
#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"

#include "alloc/cellref.h"

#include <unordered_map>

namespace lliby
{
namespace dynamic
{

class ParameterProcedureCell;

/**
 * Represents a dynamic state created by (parameterize)
 *
 * This can be viewed as a parallel stack the the program's call stack. Arbitrary values can be attached to the state
 * with (make-parameter) and (parameterize)
 */
class State
{
public:
	typedef std::unordered_map<ParameterProcedureCell*, AnyCell*> ParameterValueMap;

	/**
	 * Creates a new state with a specified parent and before/after procedures
	 *
	 * @param  parent  Pointer to the parent state or nullptr if this is a root state.
	 */
	State(State *parent = nullptr);

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
	 * Returns a pointer to our parent state or nullptr if this is the root state
	 */
	State *parent() const
	{
		return m_parent;
	}

	/**
	 * Returns the values of this state, excluding any ancestor states
	 *
	 * This is intended for use by the garbage collector
	 */
	const ParameterValueMap& selfValues() const
	{
		return m_selfValues;
	}

	/**
	 * Sets the parameter values of this state
	 *
	 * This is intended for use by the garbage collector
	 */
	void setSelfValues(const ParameterValueMap &newValues)
	{
		m_selfValues = newValues;
	}

	/**
	 * Returns the currently active state for this world
	 */
	static State* activeState(World &);

	/**
	 * Creates a child active of the currently active state and makes it active
	 *
	 * @param  world   World the state is being pushed in to
	 */
	static void pushActiveState(World &world);

	/**
	 * Makes the parent of the currently active state active
	 *
	 * @param  world   World the state is being popped from
	 *
	 * This may re-enter Scheme and invoke the garbage collector
	 */
	static void popActiveState(World &world);

	/**
	 * Switches to the specified state cell
	 *
	 * All "after" procedures for exiting states will be called followed by all "before" procedures for entering states.
	 */
	static void popUntilState(World &world, State *);

private:
	State *m_parent;
	ParameterValueMap m_selfValues;
};

}
}

#endif

