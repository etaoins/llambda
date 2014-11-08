#ifndef _LLIBY_ALLOC_ABSTRACTREF_H
#define _LLIBY_ALLOC_ABSTRACTREF_H

#include <list>
#include <vector>
#include <type_traits>

#include "binding/AnyCell.h"
#include "alloc/AllocCell.h"
#include "alloc/CellRootList.h"

namespace lliby
{
namespace alloc
{

/**
 * Abstract GC root of a contiguous range of GC managed values
 *
 * Note that unlike AbstractRef this does not maintain a copy of the referenced values. This has two important
 * implications:
 * - The referenced memory range must be valid for the lifetime of the AbstractRoot.
 * - The same memory range must not rooted by multiple AbstractRoots when the garbage collector in entered.
 *
 * \sa StrongRoot
 */
template<class T>
class AbstractRoot
{
protected:
	static_assert(std::is_base_of<AnyCell, T>(), "Only AnyCell subclasses can be GC roots");

	/**
	 * Creates a new instance rooting a cell or cell array
	 *
	 * @param rootList   List to add the root to
	 * @param cellRef    Reference to the cell or cell array
	 * @param cellCount  Number of cells in the cell array
	 */
	AbstractRoot(CellRootList *rootList, T** cellRef, size_t cellCount = 1) :
		m_rootList(rootList)
	{
		m_node.basePointer = reinterpret_cast<AllocCell**>(cellRef);
		m_node.cellCount = cellCount;

		m_rootList->addNode(m_node);
	}

	AbstractRoot(const AbstractRoot &) = delete;

	/**
	 * Destroys the GC root object and unroots the cells originally passed to the constructor
	 */
	~AbstractRoot()
	{
		m_rootList->removeNode(m_node);
	}

	CellRootList *m_rootList;
	CellRootListNode m_node;
};

/**
 * Abstract reference to a single GC managed value
 */
template<class T>
class AbstractRef : public AbstractRoot<T>
{
public:
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
	AbstractRef(CellRootList *rootList) : AbstractRef(rootList, nullptr)
	{
	}

	AbstractRef(CellRootList *rootList, T* cell) :
		AbstractRoot<T>(rootList, &m_cell, 1),
		m_cell(cell)
	{
	}

	AbstractRef(const AbstractRef &other) :
		AbstractRef(other.m_rootList, other.data())
	{
	}

private:
	T *m_cell;
};

}
}

#endif

