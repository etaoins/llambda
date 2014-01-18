#ifndef _LLIBY_ALLOC_ABSTRACTREF_H
#define _LLIBY_ALLOC_ABSTRACTREF_H

#include <list>
#include <vector>
#include <type_traits>

#include "binding/DatumCell.h"
#include "alloc/AllocCell.h"

namespace lliby
{
namespace alloc
{

/**
 * Represents a reference to a range of allocated cells
 */
struct AllocCellRefRange
{
	AllocCell **basePointer;
	size_t cellCount;
};

typedef std::list<AllocCellRefRange> CellRefList;

/**
 * Abstract reference to a range of GC managed values
 *
 * \sa StrongRefRange
 */
template<class T, CellRefList& refList>
class AbstractRefRange
{
protected:
	static_assert(std::is_base_of<DatumCell, T>(), "Only DatumCell subclasses can be GC roots");

	/**
	 * Creates an cell ref with an existing ref list iterator
	 *
	 * This is only intended for use by move constructors
	 */
	AbstractRefRange(CellRefList::iterator refListIt) :
		mRefListIt(refListIt)
	{
	}

	/**
	 * Creates a new instance rooting a cell or cell array
	 *
	 * @param cellRef    Reference to the cell or cell array
	 * @param cellCount  Number of cells in the cell array
	 */
	AbstractRefRange(T** cellRef, size_t cellCount)
	{
		// Order doesn't matter here
		const AllocCellRefRange cellRange {
			.basePointer = reinterpret_cast<AllocCell**>(cellRef),
			.cellCount = cellCount
		};

		mRefListIt = refList.insert(refList.begin(), cellRange);
	}

	/**
	 * Creates a new instance rooting a cell vector
	 *
	 * @param cellVector  Vector of cells to root. The original vector will be referenced and possibly modified if
	 *                    the garbage collector runs. Therefore it is unsafe to destroy or resize the vector while
	 *                    it is rooted.
	 */ 
	AbstractRefRange(std::vector<T*> &cellVector) :
		AbstractRefRange(cellVector.data(), cellVector.size())
	{
	}

	AbstractRefRange(const AbstractRefRange &) = delete;

	/**
	 * Destroys the GC root object and unroots the cells originally passed to the constructor
	 */
	~AbstractRefRange()
	{
		if (mRefListIt != refList.end())
		{
			refList.erase(mRefListIt);
		}
	}

	CellRefList::iterator mRefListIt;
};

/**
 * Abstract reference to a single GC managed value
 */
template<class T, CellRefList& refList>
class AbstractRef : public AbstractRefRange<T, refList>
{
public:
	/**
	 * Moves an existing AbstractRef value to this one
	 *
	 * This avoids list operations on the target CellRefList by using the existing value's list element
	 */
	AbstractRef(AbstractRef &&other) : 
		AbstractRefRange<T, refList>(other.mRefListIt),
		mCell(other.mCell)
	{
		// Update the base pointer to point to our member variable
		this->mRefListIt->basePointer = reinterpret_cast<AllocCell**>(&mCell);

		// Make sure the original value doesn't try to remove itself
		other.mRefListIt = refList.end();
	}

	operator T*() const
	{
		return mCell;
	}
	
	T* data() const
	{
		return mCell;
	}

	void setData(T* newCell)
	{
		mCell = newCell;
	}

	T* operator->() const
	{
		return mCell;
	}

	bool isNull() const
	{
		return mCell == nullptr;
	}
	
	bool operator!() const
	{
		return isNull();
	}

	operator bool() const
	{
		return !isNull();
	}

protected:
	AbstractRef() : AbstractRef(nullptr)
	{
	}

	AbstractRef(T* cell) :
		AbstractRefRange<T, refList>(&mCell, 1),
		mCell(cell)
	{
	}

private:
	T *mCell;
};

}
}

#endif

