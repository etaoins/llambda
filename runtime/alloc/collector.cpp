#include "alloc/collector.h"

#include <cstring>

#include "alloc/StrongRef.h"
#include "alloc/WeakRef.h"
#include "alloc/GarbageState.h"
#include "alloc/AllocCell.h"
#include "alloc/cellvisitor.h"
#include "binding/DatumCell.h"

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

	// Vists every non-null cell in a cell ref list
	void visitCellRefList(const CellRefList &cellRefList, std::function<bool (DatumCell**)> &visitor)
	{
		for(auto cellRefRange : cellRefList)
		{
			// Visit each cell in this range
			for(size_t i = 0; i < cellRefRange.cellCount; i++)
			{
				auto datumCellRef = reinterpret_cast<DatumCell**>(&cellRefRange.basePointer[i]);

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

void* collect(void *fromBase, void *fromEnd, void *toBase)
{
	// This needs to be an AllocCell so ++ works correctly
	auto nextNewCell = static_cast<AllocCell*>(toBase);

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
		DatumCell *newCellLocation = nextNewCell++;
		memcpy(newCellLocation, oldCellLocation, sizeof(AllocCell));

		// Update the reference to it
		*cellRef = newCellLocation;

		// Make the old cell a forwarding cell
		new (oldCellLocation) ForwardingCell(newCellLocation);

		// Visit the cell's children
		return true;
	};

	// Visit each runtime GC root
	visitCellRefList(RuntimeStrongRefList, rootVisitor);

	// Visit each runtime weak ref
	std::function<bool (DatumCell**)> weakRefFunction = weakRefVisitor;
	visitCellRefList(RuntimeWeakRefList, weakRefFunction);

	// This is where new allocations should start from
	return nextNewCell;
}

}
}
