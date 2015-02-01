#include "core/World.h"

#include "core/error.h"

#include "alloc/allocator.h"
#include "alloc/Finalizer.h"

#include "actor/Mailbox.h"

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

World::World() : activeStateCell(&sharedRootStateCell)
{
}

World::~World()
{
#ifdef _LLIBY_CHECK_LEAKS
	if (alloc::forceCollection(*this) > 0)
	{
		fatalError("Cells leaked from world on exit");
	}
#else
	// Don't bother collecting; just finalize the heap
	alloc::Finalizer::finalizeHeapSync(cellHeap);
#endif
}

void World::run(const std::function<void(World &)> &func)
{
	char stackCanary;
	continuationBase = &stackCanary;

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
