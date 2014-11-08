#ifndef _LLIBY_ALLOC_CELLROOTLIST_H
#define _LLIBY_ALLOC_CELLROOTLIST_H

#include "alloc/AllocCell.h"

#include <cstdint>

namespace lliby
{
namespace alloc
{

/**
 * Represents a reference to a range of allocated cells
 */
class CellRootListNode
{
public:
	CellRootListNode *prev;
	CellRootListNode *next;

	AllocCell **basePointer;
	size_t cellCount;
};

/**
 * List of GC roots
 *
 * The list structure is implemented using internal pointers on CellRoot itself. This means that CellRootList only
 * consists of a pointer to the head of the list.
 */
class CellRootList
{
public:
	CellRootListNode *head() const
	{
		return m_head;
	}

	void addNode(CellRootListNode &node);
	void removeNode(CellRootListNode &node);

	void relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd);

private:
	CellRootListNode *m_head = nullptr;
};

}
}

#endif

