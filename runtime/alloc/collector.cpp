#include "alloc/collector.h"

#include <cstring>

#include "alloc/StrongRef.h"
#include "alloc/WeakRef.h"
#include "alloc/GarbageState.h"
#include "alloc/AllocCell.h"
#include "alloc/cellvisitor.h"

#include "binding/DatumCell.h"

#include "dynamic/State.h"

extern "C"
{
	struct FrameMap 
	{
		std::int32_t NumRoots;    //< Number of roots in stack frame.
		std::int32_t NumMeta;     //< Number of metadata entries.  May be < NumRoots.
		const void *Meta[0]; //< Metadata for each root.
	};

	struct StackEntry
	{
		StackEntry *Next;    //< Link to next stack entry (the caller's).
		const FrameMap *Map; //< Pointer to constant FrameMap.
		void *Roots[0];      //< Stack roots (in-place array).
	};

	// This is provided by LLVM's shadowstack GC plugin
	StackEntry *llvm_gc_root_chain;
}

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
	class ForwardingCell : public DatumCell
	{
	public:
		ForwardingCell(DatumCell *newLocation) :
			DatumCell(CellTypeId::Unit, GarbageState::ForwardingCell),
			m_newLocation(newLocation)
		{
		}

		DatumCell* newLocation() const
		{
			return m_newLocation;
		}

	private:
		DatumCell *m_newLocation;
	};

	// Visit every non-null cell in a cell ref list
	void visitCellRefList(const CellRefRangeList *cellRefList, std::function<bool (DatumCell**)> &visitor)
	{
		for(auto cellRefRange = cellRefList->activeHead();
		    cellRefRange != nullptr;
		    cellRefRange = cellRefRange->next)
		{
			// Visit each cell in this range
			for(size_t i = 0; i < cellRefRange->cellCount; i++)
			{
				auto datumCellRef = reinterpret_cast<DatumCell**>(&cellRefRange->basePointer[i]);

				if (*datumCellRef != nullptr)
				{
					visitCell(datumCellRef, visitor);
				}
			}
		}
	}

	// For updating weak refs
	bool weakRefVisitor(DatumCell **cellRef)
	{
		DatumCell *oldCellLocation = *cellRef;

		if (oldCellLocation->gcState() == GarbageState::GlobalConstant)
		{
			return false;
		}

		if (oldCellLocation->gcState() == GarbageState::ForwardingCell)
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

	std::function<bool (DatumCell**)> rootVisitor = [&] (DatumCell **cellRef) -> bool
	{
		DatumCell *oldCellLocation = *cellRef;

		if (oldCellLocation->gcState() == GarbageState::GlobalConstant)
		{
			// This is a constant; don't visit it or its children
			return false;
		}

		if (oldCellLocation->gcState() == GarbageState::ForwardingCell)
		{
			// This has already been moved to the new semi-space
			// Update the reference and stop visiting
			*cellRef = static_cast<ForwardingCell*>(oldCellLocation)->newLocation(); 
			return false;
		}

		// Move the cell to the new location
		DatumCell *newCellLocation = static_cast<DatumCell*>(newHeap.allocate(1));
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
	for(StackEntry *stackEntry = llvm_gc_root_chain;
		 stackEntry != nullptr;
		 stackEntry = stackEntry->Next)
	{
		for(std::int32_t i = 0; i < stackEntry->Map->NumRoots; i++)
		{
			auto datumCellRef = reinterpret_cast<DatumCell**>(&stackEntry->Roots[i]);

			if (*datumCellRef != nullptr)
			{
				visitCell(datumCellRef, rootVisitor);
			}
		}
	}

	// Visit the dynamic state
	// XXX: In theory if a parameter function isn't referenced it's safe to remove it from all states. However, because
	// parameter values can themselves reference other parameter functions this gets extremely tricky.  Parameterization
	// of an unreachable parameter seems like too much of a corner case to justify the additional code complexity.
	visitDynamicState(dynamic::State::activeState(world), rootVisitor);

	// Visit each runtime weak ref
	std::function<bool (DatumCell**)> weakRefFunction = weakRefVisitor;
	visitCellRefList(world.weakRefs, weakRefFunction);

	return reachableCells;
}

}
}
