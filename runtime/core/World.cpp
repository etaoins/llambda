#include "core/World.h"

#include "core/error.h"

#include "alloc/allocator.h"
#include "alloc/Finalizer.h"

#include "actor/Mailbox.h"
#include "actor/ActorContext.h"

#include "binding/DynamicStateCell.h"

#include "dynamic/State.h"
#include "dynamic/SchemeException.h"

using namespace lliby;

namespace
{
	dynamic::State sharedRootState(nullptr, nullptr, nullptr);
	DynamicStateCell sharedRootStateCell(&sharedRootState, GarbageState::GlobalConstant);
}

namespace lliby
{

World::World() :
	cellHeap(InitialHeapSegmentSize),
	m_activeStateCell(&sharedRootStateCell)
{
}

World::~World()
{
	// Note that the mailbox itself is reference counted and can go away later
	delete m_actorContext;
	m_actorContext = nullptr;

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

void World::run(const std::function<void(World &)> &func)
{
	char stackCanary;
	m_continuationBase = &stackCanary;

	m_runSequence++;

	try
	{
		func(*this);
	}
	catch (dynamic::SchemeException &except)
	{
		// Call all unwind handlers
		dynamic::State::popAllStates(*this);
		throw;
	}

	dynamic::State::popAllStates(*this);
}

void World::addChildActor(std::weak_ptr<actor::Mailbox> childActor)
{
	m_childActors.push_back(childActor);
}

}
