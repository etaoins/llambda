#ifndef _LLIBY_ALLOC_ABSTRACTREF_H
#define _LLIBY_ALLOC_ABSTRACTREF_H

#include <list>
#include <vector>
#include <type_traits>

#include "binding/DatumCell.h"
#include "alloc/AllocCell.h"
#include "alloc/CellRefRangeList.h"

namespace lliby
{
namespace alloc
{

/**
 * Abstract reference to a range of GC managed values
 *
 * \sa StrongRefRange
 */
template<class T>
class AbstractRefRange
{
protected:
	static_assert(std::is_base_of<DatumCell, T>(), "Only DatumCell subclasses can be GC roots");

	/**
	 * Creates an cell ref with an existing ref range
	 *
	 * This is only intended for use by move constructors
	 */
	AbstractRefRange(CellRefRangeList *refList, CellRefRange *refRange) :
		m_refList(refList),
		m_refRange(refRange)
	{
	}

	/**
	 * Creates a new instance rooting a cell or cell array
	 *
	 * @param refList    List to add the ref range to
	 * @param cellRef    Reference to the cell or cell array
	 * @param cellCount  Number of cells in the cell array
	 */
	AbstractRefRange(CellRefRangeList *refList, T** cellRef, size_t cellCount) :
		m_refList(refList)
	{
		m_refRange = m_refList->addRange(
				reinterpret_cast<AllocCell**>(cellRef),
				cellCount
		);
	}

	/**
	 * Creates a new instance rooting a cell vector
	 *
	 * @param refList     List to add the ref range to
	 * @param cellVector  Vector of cells to root. The original vector will be referenced and possibly modified if
	 *                    the garbage collector runs. Therefore it is unsafe to destroy or resize the vector while
	 *                    it is rooted.
	 */ 
	AbstractRefRange(CellRefRangeList *refList, std::vector<T*> &cellVector) :
		AbstractRefRange(refList, cellVector.data(), cellVector.size())
	{
	}

	AbstractRefRange(const AbstractRefRange &) = delete;

	/**
	 * Destroys the GC root object and unroots the cells originally passed to the constructor
	 */
	~AbstractRefRange()
	{
		if (m_refRange != nullptr)
		{
			m_refList->removeRange(m_refRange);
		}
	}

	CellRefRangeList *m_refList;
	CellRefRange *m_refRange;
};

/**
 * Abstract reference to a single GC managed value
 */
template<class T>
class AbstractRef : public AbstractRefRange<T>
{
public:
	/**
	 * Moves an existing AbstractRef value to this one
	 *
	 * This avoids list operations on the target CellRefList by using the existing value's list element
	 */
	AbstractRef(alloc::CellRefRangeList *refList, AbstractRef &&other) : 
		AbstractRefRange<T>(refList, other.m_refRange),
		m_cell(other.m_cell)
	{
		// Update the base pointer to point to our member variable
		this->m_refRange->basePointer = reinterpret_cast<AllocCell**>(&m_cell);

		// Make sure the original value doesn't try to remove itself
		other.m_refRange = nullptr;
	}

	operator T*() const
	{
		return m_cell;
	}
	
	T* data() const
	{
		return m_cell;
	}

	void setData(T* newCell)
	{
		m_cell = newCell;
	}

	T* operator->() const
	{
		return m_cell;
	}

	bool isNull() const
	{
		return m_cell == nullptr;
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

	AbstractRef(CellRefRangeList *refList, T* cell) :
		AbstractRefRange<T>(refList, &m_cell, 1),
		m_cell(cell)
	{
	}

	AbstractRef(const AbstractRef &other) :
		AbstractRef(other.m_refList, other.data())
	{
	}

private:
	T *m_cell;
};

}
}

#endif

