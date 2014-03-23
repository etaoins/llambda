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
	CellRefRange *next;
	AllocCell **basePointer;
	size_t cellCount;
	
private:
	// This is used internally by CellRefRangeList to allow AbstractRefs to be destructed out of order
	// codegen produces singly linked ranges bacause its ranges are always added and removed in stack order
	// Make prev private to make sure it's not used outside of CellRefRangeList
	CellRefRange *prev;
};

/**
 * Custom list of CellRefRanges backed by a memory pool
 *
 * This was measured to provide a 10x speedup versus using std::list<{AllocCell **, size_t}>. One drawback is only 
 * 128 ranges are supported on 64bit (256 on 32bit). If that becomes an issue this code can be re-evaluated.
 */
class CellRefRangeList
{
public:
	CellRefRange *head() const
	{
		return mHead;
	}

	void addRange(CellRefRange &range);
	void removeRange(CellRefRange &range);

private:
	CellRefRange *mHead = nullptr;
};

}
}

#endif

