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

class Continuation
{
public:
	struct CaptureResult
	{
		Continuation *continuation;
		AnyCell *passedValue;
	};

	static CaptureResult capture(World &world); 
	void resume(World &world, AnyCell *passedValue);

	alloc::ShadowStackEntry* shadowStackHead() const
	{
		return m_shadowStackHead;
	}

	const alloc::CellRefRangeList& strongRefs() const
	{
		return m_strongRefs;
	}
	
	const alloc::CellRefRangeList& weakRefs() const
	{
		return m_weakRefs;
	}

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

