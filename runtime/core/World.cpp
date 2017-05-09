#include "core/World.h"

#include "core/error.h"

#include "alloc/allocator.h"
#include "alloc/Finalizer.h"

#include "actor/Mailbox.h"
#include "actor/ActorContext.h"

#include "dynamic/State.h"

using namespace lliby;

namespace
{
	dynamic::State sharedRootState(nullptr);
}

namespace lliby
{

World::World() :
	cellHeap(InitialHeapSegmentSize),
	m_activeState(&sharedRootState)
{
}

World::~World()
{
	// Note that the mailbox itself is reference counted and can go away later
	delete m_actorContext;
	m_actorContext = nullptr;

	// TODO: We should free our cell heap here

	// Wait for our children to stop
	for(auto weakChildActor : m_childActors)
	{
		std::shared_ptr<actor::Mailbox> childActor(weakChildActor.lock());

		if (childActor)
		{
			childActor->requestLifecycleAction(actor::LifecycleAction::Stop);
			childActor->waitForStop();
		}
	}

#ifdef _LLIBY_CHECK_LEAKS
	if (alloc::forceCollection(*this) > 0)
	{
		fatalError("Cells leaked from world on exit");
	}
#endif
}

void World::addChildActor(const std::weak_ptr<actor::Mailbox> &childActor)
{
	m_childActors.push_back(childActor);
}

}
