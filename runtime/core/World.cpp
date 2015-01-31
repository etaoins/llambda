#include "core/World.h"

#include "core/error.h"

#include "alloc/CellRootList.h"
#include "alloc/allocator.h"

#include "actor/Mailbox.h"

#include "binding/DynamicStateCell.h"

#include "dynamic/State.h"
#include "dynamic/SchemeException.h"
#include "dynamic/State.h"

using namespace lliby;

namespace
{
	dynamic::State sharedRootState(nullptr, nullptr, nullptr);
	DynamicStateCell sharedRootStateCell(&sharedRootState, GarbageState::GlobalConstant);
}

namespace lliby
{

World::World() : activeStateCell(&sharedRootStateCell)
{
}

World::~World()
{
}

void World::launchWorld(void (*entryPoint)(World &))
{
	World world;

	alloc::initWorld(world);
	world.continuationBase = &world;

	try
	{
		entryPoint(world);
	}
	catch (dynamic::SchemeException &except)
	{
		// Call all unwind handlers
		dynamic::State::popAllStates(world);
		fatalError("Unhandled exception", except.object());
	}

	dynamic::State::popAllStates(world);
	alloc::shutdownWorld(world, true);
}

const std::shared_ptr<actor::Mailbox>& World::mailbox() const
{
	// Lazily initialise the mailbox
	if (!m_mailbox)
	{
		m_mailbox.reset(new actor::Mailbox);
	}

	return m_mailbox;
}

}

extern "C"
{

void llcore_launch_world(void (*entryPoint)(World &))
{
	World::launchWorld(entryPoint);
}

}
