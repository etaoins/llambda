#include "alloc/collector.h"

#include <cstring>
#include <cassert>

#include "core/World.h"

#include "alloc/GarbageState.h"
#include "alloc/AllocCell.h"
#include "alloc/CellRefWalker.h"
#include "alloc/CellRootList.h"
#include "alloc/Heap.h"

#include "actor/ActorContext.h"

#include "binding/AnyCell.h"

#include "dynamic/State.h"

namespace lliby
{
namespace alloc
{

namespace
{
	/**
	 * Internal class to store the new location of relocated cells
	 *
	 * These should not be reachable outside of the garbage collector
	 */
	class ForwardingCell : public AnyCell
	{
	public:
		ForwardingCell(AnyCell *newLocation) :
			AnyCell(CellTypeId::Invalid, GarbageState::ForwardingCell),
			m_newLocation(newLocation)
		{
		}

		AnyCell* newLocation() const
		{
			return m_newLocation;
		}

	private:
		AnyCell *m_newLocation;
	};

	// For updating weak refs
	bool weakRefVisitor(AnyCell **cellRef)
	{
		AnyCell *oldCellLocation = *cellRef;

		if (oldCellLocation->gcState() == GarbageState::GlobalConstant)
		{
			return false;
		}
		else if (oldCellLocation->gcState() == GarbageState::ForwardingCell)
		{
			// This was moved; updated the reference
			*cellRef = static_cast<ForwardingCell*>(oldCellLocation)->newLocation();

			return false;
		}

		// No longer reachable
		*cellRef = nullptr;
		return false;
	};
}

size_t collect(World &world, Heap &newHeap)
{
	size_t reachableCells = 0;

	CellRefWalker walker;

	auto rootVisitor = [&] (AnyCell **cellRef) -> bool
	{
		AnyCell *oldCellLocation = *cellRef;
		GarbageState gcState = oldCellLocation->gcState();

		if (gcState == GarbageState::GlobalConstant)
		{
			// This is managed by the compiler; don't visit it or its children
			return false;
		}
		else if (gcState == GarbageState::ForwardingCell)
		{
			// This has already been moved to the new semi-space
			// Update the reference and stop visiting
			*cellRef = static_cast<ForwardingCell*>(oldCellLocation)->newLocation();
			return false;
		}
		else if (gcState == GarbageState::StackAllocatedCell)
		{
			// Stack allocated cells may reference heap allocated cells but they must not be relocated themselves.
			// Visit our children but skip the below relocation logic.
			return true;
		}

		// It must be HeapAllocatedCell otherwise we have memory corruption
		assert(gcState == GarbageState::HeapAllocatedCell);

		// Move the cell to the new location
		AnyCell *newCellLocation = static_cast<AnyCell*>(newHeap.allocate(1));
		memcpy(newCellLocation, oldCellLocation, sizeof(AllocCell));

		// Update the reference to it
		*cellRef = newCellLocation;

		// Make the old cell a forwarding cell
		new (oldCellLocation) ForwardingCell(newCellLocation);

		// Track this as reachable
		reachableCells++;

		// Visit the cell's children
		return true;
	};

	// Visit each runtime GC root
	walker.visitCellRootList(world.strongRoots(), rootVisitor);

	// Visit each compiler GC root
	walker.visitShadowStack(world.shadowStackHead, rootVisitor);

	// Visit the dynamic state
	walker.visitDynamicState(world.activeState(), rootVisitor);

	// Is this world an actor?
	if (world.actorContext())
	{
		walker.visitCell(reinterpret_cast<AnyCell**>(world.actorContext()->closureRef()), rootVisitor);

		if (world.actorContext()->behaviour())
		{
			walker.visitCell(reinterpret_cast<AnyCell**>(world.actorContext()->behaviourRef()), rootVisitor);
		}

		if (world.actorContext()->supervisorStrategy())
		{
			walker.visitCell(reinterpret_cast<AnyCell**>(world.actorContext()->supervisorStrategyRef()), rootVisitor);
		}
	}

	// Visit each runtime weak root
	walker.visitCellRootList(world.weakRoots(), weakRefVisitor);

	return reachableCells;
}

}
}
