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
	AbstractRoot(const CellRootIterable *rootList, T** cellRef, std::size_t cellCount = 1)
	{
		m_node.setData(reinterpret_cast<AllocCell**>(cellRef), cellCount);
		m_node.insertAfter(rootList);
	}

	AbstractRoot(const AbstractRoot &) = delete;

	/**
	 * Destroys the GC root object and unroots the cells originally passed to the constructor
	 */
	~AbstractRoot()
	{
		m_node.remove();
	}

	ExternalRootListNode m_node;
};

/**
 * Abstract reference to a single GC managed value
 */
template<class T>
class AbstractRef
{
public:
	T* data() const
	{
		return static_cast<T*>(m_node.cell());
	}

	operator T*() const
	{
		return data();
	}

	void setData(T* newCell)
	{
		m_node.setCell(newCell);
	}

	T* operator->() const
	{
		return data();
	}

	bool isNull() const
	{
		return data() == nullptr;
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
	AbstractRef(const CellRootIterable *rootList, T* cell)
	{
		m_node.setCell(cell);
		m_node.insertAfter(rootList);
	}

	AbstractRef(const CellRootIterable *rootList) : AbstractRef(rootList, nullptr)
	{
	}

	AbstractRef(const AbstractRef &other) :
		AbstractRef(&other.m_node, other.data())
	{
	}

	~AbstractRef()
	{
		m_node.remove();
	}

private:
	InternalRootListNode m_node;
};

}
}

#endif

