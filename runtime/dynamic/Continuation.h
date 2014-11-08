#ifndef _LLIBY_DYNAMIC_CONTINUATION_H
#define _LLIBY_DYNAMIC_CONTINUATION_H

#include <csetjmp>

#include "binding/AnyCell.h"
#include "binding/ProperList.h"
#include "alloc/StrongRef.h"
#include "alloc/cellref.h"
#include "alloc/CellRootList.h"

namespace lliby
{

class World;

namespace dynamic
{

/**
 * Represents a continuation
 *
 * This is used to implement (call/cc). Continuation instances are typically wrapped in EscapeProcedureCell instances 
 * to both make them visible to the garbage collector and applicable from Scheme.
 */
class Continuation
{
public:
	/**
	 * Captures the current continuation
	 *
	 * This function can appear to return multiple times. It will return once normally with a null passedValue(). Calls
	 * to resume() will cause additional returns from capture() where passedValue() returns the cell passed to resume().
	 */
	static Continuation* capture(World &world); 

	/**
	 * Resumes the continuation passing the specified value
	 *
	 * This will cause the original capture() call to return again on a copy of its original stack
	 */
	void resume(World &world, ProperList<AnyCell> *passedValues);

	/**
	 * Returns the captured value passed to resume() value and sets it to null
	 *
	 * This should be used by callers of capture() to retrieve the passed value
	 */
	ProperList<AnyCell> *takePassedValues()
	{
		auto ret = m_passedValues;
		m_passedValues = nullptr;
		return ret;
	}

	// This is used by the GC to update our passed value pointer
	ProperList<AnyCell>** passedValuesRef()
	{
		return &m_passedValues;
	}

	/**
	 * Pointer to the shadow stack of this continuation
	 *
	 * This is duplicated from the World when the continuation is captured and then relocated to the appropriate
	 * memory location.
	 */
	alloc::ShadowStackEntry* shadowStackHead() const
	{
		return m_shadowStackHead;
	}

	/**
	 * Reference to the strong cell root list for this continuation
	 *
	 * This is duplicated from the World when the continuation is captured and then relocated to the appropriate
	 * memory location.
	 */
	const alloc::CellRootList& strongRoots() const
	{
		return m_strongRoots;
	}
	
	/**
	 * Reference to the weak cell root list for this continuation
	 *
	 * This is duplicated from the World when the continuation is captured and then relocated to the appropriate
	 * memory location.
	 */
	const alloc::CellRootList& weakRoots() const
	{
		return m_weakRoots;
	}

	/**
	 * Returns the dynamic state the continuation was initially captured in
	 */
	DynamicStateCell* dynamicStateCell() const
	{
		return m_dynamicStateCell;
	}
	
	// This is used by the garbage collector to update our dynamic state pointer
	DynamicStateCell** dynamicStateCellRef() 
	{
		return &m_dynamicStateCell;
	}

private:
	Continuation() = delete;

	/**
	 * Number of bytes of stack copied starting from World::continuationBase
	 */
	std::ptrdiff_t m_savedStackBytes;

	// These mirror	the state in the World
	alloc::ShadowStackEntry *m_shadowStackHead = nullptr;
	alloc::CellRootList m_strongRoots;
	alloc::CellRootList m_weakRoots;
	DynamicStateCell *m_dynamicStateCell;

	/**
	 * Space store values passed to a continuation
	 */
	ProperList<AnyCell> *m_passedValues;

	/**
	 * Jump target of the continuation
	 */
	std::jmp_buf m_jumpTarget;

	char m_savedStack[];
};

}
}
#endif

