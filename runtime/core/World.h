#ifndef _LLIBY_CORE_WORLD_H
#define _LLIBY_CORE_WORLD_H

#include "alloc/Heap.h"
#include "alloc/ShadowStackEntry.h"
#include "alloc/CellRootList.h"

#include <memory>
#include <functional>

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
class CellRootList;
}

namespace actor
{
class ActorContext;
}

class World
{
	friend class dynamic::Continuation;
public:
	//
	// This is the public section of World
	// Generated code can access these fields directly
	// Any changes to the content, size or order of these fields will require codegen changes
	//

	alloc::ShadowStackEntry *shadowStackHead = nullptr;
	alloc::Heap cellHeap;


public: // Normal C++ API
	/**
	 * Constructs a new non-running world
	 *
	 * @sa run()
	 */
	World();

	/**
	 * Destroys the World and frees all resources associated with it
	 */
	~World();

	/**
	 * Sets up the world to run and then calls the passed function
	 */
	void run(const std::function<void(World &)> &func);

	/**
	 * Returns the dynamic state cell containing the active dynamic state of the world
	 *
	 * This is a reference to a pointer for the GC
	 */
	DynamicStateCell*& activeStateCell()
	{
		return m_activeStateCell;
	}

	/**
	 * Returns a reference to the strong GC root list
	 */
	alloc::CellRootList& strongRoots()
	{
		return m_strongRoots;
	}

	/**
	 * Returns a reference to the weak GC root list
	 */
	alloc::CellRootList& weakRoots()
	{
		return m_weakRoots;
	}

	/**
	 * Creates a new actor context for the world
	 *
	 * This should be called once before entering the actor's receive loop to set up the world as an actor
	 *
	 * @sa actorContext()
	 */
	void createActorContext();

	/**
	 * Returns the actor context for this world or nullptr if none exists
	 */
	actor::ActorContext* actorContext()
	{
		return m_actorContext;
	}


protected: // Continuation support
	/**
	 * Returns the stack pointer to the top of the World's stack
	 */
	void* continuationBase()
	{
		return m_continuationBase;
	}

	/**
	 * Returns the current resuming continuation
	 *
	 * This is used as a temporary safe place to store the continuation pointer during resume
	 */
	volatile dynamic::Continuation* resumingContinuation()
	{
		return m_resumingContinuation;
	}

	/**
	 * Sets the current resuming continuation
	 */
	void setResumingContinuation(dynamic::Continuation *resumingContinuation)
	{
		m_resumingContinuation = resumingContinuation;
	}

private:
	DynamicStateCell *m_activeStateCell;

	alloc::CellRootList m_strongRoots;
	alloc::CellRootList m_weakRoots;

	void *m_continuationBase;

	volatile dynamic::Continuation *m_resumingContinuation;

	// This is lazily initialised on first use
	actor::ActorContext *m_actorContext = nullptr;
};

}

#endif
