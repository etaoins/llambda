#include "alloc/collector.h"

#include <cstring>

#include "core/World.h"

#include "alloc/GarbageState.h"
#include "alloc/AllocCell.h"
#include "alloc/cellvisitor.h"
#include "alloc/CellRefRangeList.h"
#include "alloc/Heap.h"

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

	// Visit every non-null cell in a cell ref list
	void visitCellRefList(const CellRefRangeList *cellRefList, std::function<bool (AnyCell**)> &visitor)
	{
		for(auto cellRefRange = cellRefList->head();
		    cellRefRange != nullptr;
		    cellRefRange = cellRefRange->next)
		{
			// Visit each cell in this range
			for(size_t i = 0; i < cellRefRange->cellCount; i++)
			{
				auto cellRef = reinterpret_cast<AnyCell**>(&cellRefRange->basePointer[i]);

				if (*cellRef != nullptr)
				{
					visitCell(cellRef, visitor);
				}
			}
		}
	}

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

	std::function<bool (AnyCell**)> rootVisitor = [&] (AnyCell **cellRef) -> bool
	{
		AnyCell *oldCellLocation = *cellRef;

		if (oldCellLocation->gcState() == GarbageState::GlobalConstant)
		{
			// This is a constant; don't visit it or its children
			return false;
		}
		else if (oldCellLocation->gcState() == GarbageState::ForwardingCell)
		{
			// This has already been moved to the new semi-space
			// Update the reference and stop visiting
			*cellRef = static_cast<ForwardingCell*>(oldCellLocation)->newLocation(); 
			return false;
		}
		else
		{
			// It must be AllocatedCell otherwise we have memory corruption
			assert(oldCellLocation->gcState() == GarbageState::AllocatedCell);
		}

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
	visitCellRefList(world.strongRefs, rootVisitor);

	// Visit each compiler GC root
	for(ShadowStackEntry *stackEntry = world.shadowStackHead;
		 stackEntry != nullptr;
		 stackEntry = stackEntry->next)
	{
		for(std::uint64_t i = 0; i < stackEntry->cellCount; i++)
		{
			auto cellRef = &stackEntry->roots[i];

			if (*cellRef != nullptr)
			{
				visitCell(cellRef, rootVisitor);
			}
		}
	}

	// Visit the dynamic state
	// XXX: In theory if a parameter function isn't referenced it's safe to remove it from all states. However, because
	// parameter values can themselves reference other parameter functions this gets extremely tricky.  Parameterization
	// of an unreachable parameter seems like too much of a corner case to justify the additional code complexity.
	visitDynamicState(dynamic::State::activeState(world), rootVisitor);

	// Visit each runtime weak ref
	std::function<bool (AnyCell**)> weakRefFunction = weakRefVisitor;
	visitCellRefList(world.weakRefs, weakRefFunction);

	return reachableCells;
}

}
}
