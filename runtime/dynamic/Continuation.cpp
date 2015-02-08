#include "dynamic/Continuation.h"

#include <cassert>
#include <cstdlib>
#include <cstring>

#include "binding/RecordLikeCell.h"
#include "dynamic/State.h"

namespace lliby
{
namespace dynamic
{

namespace
{
	const int ContinuationResumeCookie = 1;

	void relocateShadowStackEntry(alloc::ShadowStackEntry *&stackEntry, std::ptrdiff_t offset)
	{
		if (stackEntry != nullptr)
		{
			// Cast to char* so we do byte-based pointer arithmetic
			stackEntry = reinterpret_cast<alloc::ShadowStackEntry*>(reinterpret_cast<char*>(stackEntry) + offset);
		}
	}

	void relocateShadowStack(alloc::ShadowStackEntry *&stackHead, std::ptrdiff_t offset)
	{
		if (stackHead != nullptr)
		{
			relocateShadowStackEntry(stackHead, offset);

			for(alloc::ShadowStackEntry *entry = stackHead;
				entry != nullptr;
				entry = entry->next)
			{
				relocateShadowStackEntry(entry->next, offset);
			}
		}
	}
}

Continuation* Continuation::capture(World &world)
{
	void *stackPointer = &stackPointer;

	ptrdiff_t stackSize = static_cast<char*>(world.continuationBase()) - static_cast<char*>(stackPointer);
	assert(stackSize > 0);

	// Allocate a continuation
	// Use allocatedRecorData because this is used as the closure for EscapeProcedureCell
	auto *cont = reinterpret_cast<Continuation*>(RecordLikeCell::allocateRecordData(sizeof(Continuation) + stackSize));

	// Save our run sequence number
	cont->m_runSequence = world.runSequence();

	// Copy the entire stack over
	memcpy(cont->m_savedStack, static_cast<char*>(world.continuationBase()) - stackSize, stackSize);

	// Record the stack size
	cont->m_savedStackBytes = stackSize;

	// Calculate the offset between the old stack and the new saved heap
	std::ptrdiff_t relocationOffset = &cont->m_savedStack[stackSize] - static_cast<char*>(world.continuationBase());

	// Save metadata from the world
	cont->m_shadowStackHead = world.shadowStackHead;
	relocateShadowStack(cont->m_shadowStackHead, relocationOffset);

	cont->m_strongRoots = world.strongRoots();
	cont->m_strongRoots.relocate(relocationOffset, stackPointer, world.continuationBase());

	cont->m_weakRoots = world.weakRoots();
	cont->m_weakRoots.relocate(relocationOffset, stackPointer, world.continuationBase());

	cont->m_dynamicStateCell = world.activeStateCell();

	// We don't have a passed value yet
	cont->m_passedValues = nullptr;

	// Finally set the jump target
	const int jumpResult = setjmp(cont->m_jumpTarget);

	if (jumpResult == 0)
	{
		return cont;
	}
	else if (jumpResult == ContinuationResumeCookie)
	{
		// Our stack is completely bogus for everything after "currentStackCanary"
		// We need to rebuild the values we use here
		cont = const_cast<Continuation*>(world.resumingContinuation());
		stackSize = cont->m_savedStackBytes;
		std::ptrdiff_t delocationOffset = static_cast<char*>(world.continuationBase()) - &cont->m_savedStack[stackSize];

		// Assign our GC state back in to the world and delocate it back in to place
		world.shadowStackHead = cont->m_shadowStackHead;
		relocateShadowStack(world.shadowStackHead, delocationOffset);

		world.strongRoots() = cont->m_strongRoots;
		world.strongRoots().relocate(delocationOffset, &cont->m_savedStack[0], &cont->m_savedStack[stackSize]);

		world.weakRoots() = cont->m_weakRoots;
		world.weakRoots().relocate(delocationOffset, &cont->m_savedStack[0], &cont->m_savedStack[stackSize]);

		// Switch our dynamic state
		State::switchStateCell(world, cont->m_dynamicStateCell);

		return cont;
	}
	else
	{
		// Unexpected return value from longjmp
		abort();
	}
}

bool Continuation::resume(World &world, ProperList<AnyCell> *passedValues)
{
	void *stackPointer = &stackPointer;

	if (m_runSequence != world.runSequence())
	{
		return false;
	}

	ptrdiff_t currentStackSize = static_cast<char*>(world.continuationBase()) - static_cast<char*>(stackPointer);
	assert(currentStackSize > -1);

	if (currentStackSize < m_savedStackBytes)
	{
		// We need to allocate some more stack space otherwise we'll overwrite our own stack while executing
		// This means memcpy() will have its return stack corrupted and all sorts of badness will happen. Instead just
		// alloca() enough space and re-enter ourselves to ensure our entire stack frame is safe
		alloca(m_savedStackBytes - currentStackSize);
		resume(world, passedValues);
	}

	// Track that we're the resuming continuation
	world.setResumingContinuation(this);

	// Stash our passed value inside the continuation
	m_passedValues = passedValues;

	// Copy the stack back over
	memcpy(static_cast<char*>(world.continuationBase()) - m_savedStackBytes, m_savedStack, m_savedStackBytes);

	// DO NOT PLACE CODE HERE - THE STACK IS IN AN INCONSISTENT STATE

	// Now jump back to the original location
	longjmp(m_jumpTarget, ContinuationResumeCookie);

	// Not reachable
	return true;
}

}
}
