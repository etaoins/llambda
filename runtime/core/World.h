#ifndef _LLIBY_CORE_WORLD_H
#define _LLIBY_CORE_WORLD_H

#include "alloc/Heap.h"
#include "alloc/ShadowStackEntry.h"
#include "alloc/CellRootList.h"

#include <memory>

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
class Mailbox;
}

class World
{
public:
	World();
	~World();

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

	// These are lists of strong and weak roots in the current world
	alloc::CellRootList strongRoots;
	alloc::CellRootList weakRoots;

	// This is the stack base where continuations are copied to/from
	void *continuationBase;

	// The currently resuming continuation
	// This is used as temporary storage space while a continuation resumes itself
	// This is used as temporary storage space while a continuation resumes itself
	volatile dynamic::Continuation *resumingContinuation;

	/**
	 * Returns the current mailbox for this world
	 */
	const std::shared_ptr<actor::Mailbox>& mailbox() const;

	/**
	 * Returns the mailbox for the last received message in the world
	 */
	const std::weak_ptr<actor::Mailbox>& sender()
	{
		return m_sender;
	}

	/**
	 * Sets the mailbox of rthe last received message in the world
	 */
	void setSender(const std::weak_ptr<actor::Mailbox> &sender)
	{
		m_sender = sender;
	}

private:
	// This is lazily initialised on first use
	mutable std::shared_ptr<actor::Mailbox> m_mailbox;
	std::weak_ptr<actor::Mailbox> m_sender;
};

}

extern "C"
{

void llcore_launch_world(void (*entryPoint)(lliby::World &));

}

#endif
