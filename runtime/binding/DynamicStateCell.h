#ifndef _LLIBY_BINDING_DYNAMICSTATECELL_H
#define _LLIBY_BINDING_DYNAMICSTATECELL_H

#include "AnyCell.h"

namespace lliby
{
class World;

namespace dynamic
{
class State;
}

/**
 * Garbage collected wrapper for dynamic::State
 *
 * This is a normal garbage collected cell that points directly to a dynamic::State instance. This is used to implement
 * garbage collection of states using the exisiting GC infrastructure.
 */
class DynamicStateCell : public AnyCell
{
#include "generated/DynamicStateCellMembers.h"
public:
	static DynamicStateCell* createInstance(World &world, dynamic::State *state = nullptr);
	
	void finalizeDynamicState();

	void setState(dynamic::State *state)
	{
		m_state = state;
	}

	/**
	 * Directly creates a new DynamicStateCell
	 *
	 * This is only public to allow the creation of the shared root global state
	 */
	DynamicStateCell(dynamic::State *state, GarbageState gcState) :
		AnyCell(CellTypeId::DynamicState, gcState),
		m_state(state)
	{
	}
};
	
}


#endif
