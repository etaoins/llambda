#ifndef _LLIBY_ALLOC_CELLROOTLIST_H
#define _LLIBY_ALLOC_CELLROOTLIST_H

#include "alloc/AllocCell.h"

#include <cstdint>
#include <limits>

namespace lliby
{
namespace alloc
{

class CellRootListNode;

/**
 * Base class for both cell root list nodes and the list itself
 *
 * For CellRootListNodes the next pointer is the next node in the list. For a CellRootList the next pointer is the
 * list's head.
 */
class CellRootIterable
{
	friend class CellRootListNode;
public:
	CellRootListNode* next() const
	{
		return m_next;
	}

	void relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd);

protected:
	mutable CellRootListNode *m_next;
};

/** Reference to GC rooted cells with internal linked list pointers */
class CellRootListNode : public CellRootIterable
{
public:
	/**
	 * Returns if this root node is an instance of InternalRootListNode
	 */
	bool isInternal() const
	{
		return m_cellCount == InternalCellCount;
	}

	/**
	 * Returns if this root node is an instance of ExternalRootListNode
	 */
	bool isExternal() const
	{
		return !isInternal();
	}

	/**
	 * Inserts this node after another root list iterable
	 */
	void insertAfter(const CellRootIterable *other)
	{
		// Add this to the front of the list
		m_prev = const_cast<CellRootIterable*>(other);
		m_next = other->m_next;

		if (other->m_next)
		{
			other->m_next->m_prev = this;
		}

		// This node is now the head
		other->m_next = this;
	}

	/**
	 * Removes this node from the root list
	 */
	void remove()
	{
		// We always have a previous pointer. If we're the first node then prev will point to the list itself and update
		// the "head" (aka next) pointer
		m_prev->m_next = m_next;

		if (m_next != nullptr)
		{
			m_next->m_prev = m_prev;
		}
	}

	/**
	 * Returns the previous iterable in the list
	 */
	CellRootIterable* prev() const
	{
		return m_prev;
	}

	void relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd);

protected:
	CellRootListNode()
	{
	}

	const static std::size_t InternalCellCount = std::numeric_limits<std::size_t>::max();

	mutable CellRootIterable *m_prev;
	std::size_t m_cellCount;
};

/** Reference to a single cell where the cell pointer is contained in the list node */
class InternalRootListNode : public CellRootListNode
{
public:
	InternalRootListNode()
	{
		m_cellCount = InternalCellCount;
	}

	void setCell(AnyCell *value)
	{
		m_cell = value;
	}

	AnyCell* cell() const
	{
		return m_cell;
	}

	AnyCell** cellRef() const
	{
		return const_cast<AnyCell**>(&m_cell);
	}

	void relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd);

protected:
	AnyCell *m_cell;
};

/** Reference to a range of cell pointers contained externally from the list node */
class ExternalRootListNode : public CellRootListNode
{
public:
	void setData(AllocCell **basePointer, std::size_t count)
	{
		m_basePointer = basePointer;
		m_cellCount = count;
	}

	AllocCell** basePointer() const
	{
		return m_basePointer;
	}

	std::size_t cellCount() const
	{
		return m_cellCount;
	}

	void relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd);

protected:
	AllocCell **m_basePointer;
};

/**
 * List of GC roots
 *
 * The list structure is implemented using internal pointers on CellRoot itself. This means that CellRootList only
 * consists of a pointer to the head of the list inherited from CellRootIterable.
 */
class CellRootList : public CellRootIterable
{
public:
	CellRootList()
	{
		m_next = nullptr;
	}

	CellRootListNode* head() const
	{
		return next();
	}

	void relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd);
};

}
}

#endif

