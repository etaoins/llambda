#ifndef _LLIBY_DYNAMIC_CONTINUATION_H
#define _LLIBY_DYNAMIC_CONTINUATION_H

#include <csetjmp>

#include "binding/AnyCell.h"
#include "alloc/StrongRef.h"
#include "alloc/cellref.h"
#include "alloc/CellRefRangeList.h"

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
	struct CaptureResult
	{
		/**
		 * Captured continuation object
		 *
		 * This is initialised in both the capture and resume return paths
		 */
		Continuation *continuation;

		/**
		 * Value passed to the resumed continuation
		 *
		 * This is set to nullptr in the capture return paths
		 */
		AnyCell *passedValue;
	};

	/**
	 * Captures the current continuation
	 *
	 * This function can appear to return multiple times. It will return once normally (called the capture path) and
	 * return just a Continuation* instance. Every time resume() is invoked on the returned Continuation instance this
	 * function will return an additional time on a copy of the original stack
	 */
	static CaptureResult capture(World &world); 

	/**
	 * Resumes the continuation passing the specified value
	 *
	 * This will cause the original capture() call to return again on a copy of its original stack
	 */
	void resume(World &world, AnyCell *passedValue);

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
	 * Reference to the strong cell reference list for this continuation
	 *
	 * This is duplicated from the World when the continuation is captured and then relocated to the appropriate
	 * memory location.
	 */
	const alloc::CellRefRangeList& strongRefs() const
	{
		return m_strongRefs;
	}
	
	/**
	 * Reference to the weak cell reference list for this continuation
	 *
	 * This is duplicated from the World when the continuation is captured and then relocated to the appropriate
	 * memory location.
	 */
	const alloc::CellRefRangeList& weakRefs() const
	{
		return m_weakRefs;
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
	alloc::CellRefRangeList m_strongRefs;
	alloc::CellRefRangeList m_weakRefs;
	DynamicStateCell *m_dynamicStateCell;

	/**
	 * Space store a value passed to a continuation
	 */
	AnyCell *m_passedValue;

	/**
	 * Jump target of the continuation
	 */
	std::jmp_buf m_jumpTarget;

	char m_savedStack[];
};

}
}
#endif

