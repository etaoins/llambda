#ifndef _LLIBY_CORE_WORLD_H
#define _LLIBY_CORE_WORLD_H

#include "alloc/Heap.h"

#include <memory>
#include <vector>

namespace lliby
{

namespace actor
{
class ActorContext;
class Mailbox;
}

namespace dynamic
{
class State;
}

class World
{
public:
	//
	// This is the public section of World
	// Generated code can access these fields directly
	// Any changes to the content, size or order of these fields will require codegen changes
	//

	alloc::Heap cellHeap;


public: // Normal C++ API
	/**
	 * Start World heaps with 4K allocations
	 */
	static const std::size_t InitialHeapSegmentSize = 4 * 1024;

	/**
	 * Constructs a new World
	 */
	World();

	/**
	 * Destroys the World and frees all resources associated with it
	 */
	~World();

	/**
	 * Returns the active dynamic state of the world
	 */
	dynamic::State* activeState()
	{
		return m_activeState;
	}

	/**
	 * Sets the active state for the world
	 *
	 * This is intended for use by dynamic::State
	 */
	void setActiveState(dynamic::State *state)
	{
		m_activeState = state;
	}

	/**
	 * Sets the world's actor context
	 *
	 * @sa actorContext()
	 */
	void setActorContext(actor::ActorContext *context)
	{
		m_actorContext = context;
	}

	/**
	 * Returns the actor context for this world or nullptr if none exists
	 */
	actor::ActorContext* actorContext()
	{
		return m_actorContext;
	}

	/**
	 * Adds a child actor to this world
	 *
	 * All child actors will be synchronously stopped in the destructor for the world
	 */
	void addChildActor(const std::weak_ptr<actor::Mailbox> &childActor);

private:
	dynamic::State *m_activeState;

	actor::ActorContext *m_actorContext = nullptr;
	std::vector<std::weak_ptr<actor::Mailbox>> m_childActors;
};

}

#endif
