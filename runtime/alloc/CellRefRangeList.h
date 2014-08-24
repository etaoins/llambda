#ifndef _LLIBY_ALLOC_CELLREFRANGELIST_H
#define _LLIBY_ALLOC_CELLREFRANGELIST_H

#include "alloc/AllocCell.h"

#include <cstdint>


namespace lliby
{
namespace alloc
{

class CellRefRangeList;

/**
 * Represents a reference to a range of allocated cells
 */
class CellRefRange
{
	friend class CellRefRangeList;

public:
	CellRefRange *prev;
	CellRefRange *next;
	AllocCell **basePointer;
	size_t cellCount;
};

/**
 * List of CellRefRanges
 *
 * The list structure is implemented using internal pointers on CellRefRange itself. This means that CellRefRangeList
 * only consists of a pointer to the head of the list.
 */
class CellRefRangeList
{
public:
	CellRefRange *head() const
	{
		return m_head;
	}

	void addRange(CellRefRange &range);
	void removeRange(CellRefRange &range);

	void relocate(std::ptrdiff_t offset, void *oldStackStart, void *oldStackEnd);

private:
	CellRefRange *m_head = nullptr;
};

}
}

#endif

