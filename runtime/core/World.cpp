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

World::World() : m_activeStateCell(&sharedRootStateCell)
{
}

World::~World()
{
	// Note that the mailbox itself is reference counted and can go away later
	delete m_actorContext;

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

void World::createActorContext()
{
	assert(m_actorContext == nullptr);
	m_actorContext = new actor::ActorContext;
}

}
