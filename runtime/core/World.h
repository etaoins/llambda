#ifndef _LLIBY_CORE_WORLD_H
#define _LLIBY_CORE_WORLD_H

#include "alloc/Heap.h"
#include "alloc/ShadowStackEntry.h"
#include "alloc/CellRefRangeList.h"

#include "core/World.h"

namespace lliby
{

namespace dynamic
{
class Continuation;
}

namespace alloc
{
class MemoryBlock;
class AllocCell;
class CellRefRangeList;
}

class World
{
public:
	static void launchWorld(void (*entryPoint)(World &));

	//
	// This is the public section of World
	// Generated code can access these fields directly
	// Any changes to the content, size or order of these fields will require codegen changes
	//
	
	alloc::ShadowStackEntry *shadowStackHead = nullptr;
	alloc::Heap cellHeap;

	//
	// This is the private section of World
	// This is only used internally by the runtime
	//
	
	DynamicStateCell *activeStateCell;

	// These are lists of strong and weak refs in the current world
	alloc::CellRefRangeList strongRefs;
	alloc::CellRefRangeList weakRefs;

	// This is the stack base where continuations are copied to/from
	void *continuationBase;

	// The currently resuming continuation
	// This is used as temporary storage space while a continuation resumes itself
	// This is used as temporary storage space while a continuation resumes itself
	volatile dynamic::Continuation *resumingContinuation;
	
private:
	World();
	~World();
};

}

extern "C"
{

void _lliby_launch_world(void (*entryPoint)(lliby::World &));

}

#endif
