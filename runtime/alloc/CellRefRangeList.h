#ifndef _LLIBY_ALLOC_CELLREFRANGELIST_H
#define _LLIBY_ALLOC_CELLREFRANGELIST_H

#include "alloc/AllocCell.h"

#include <cstdint>

namespace lliby
{
namespace alloc
{

class MemoryBlock;

/**
 * Represents a reference to a range of allocated cells
 */
struct CellRefRange
{
	CellRefRange *prev;
	CellRefRange *next;
	AllocCell **basePointer;
	size_t cellCount;
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
	explicit CellRefRangeList();
	~CellRefRangeList();

	CellRefRange *activeHead() const
	{
		return mActiveHead;
	}

	CellRefRange *addRange(AllocCell **basePointer, size_t cellCount);
	void removeRange(CellRefRange *range);

private:
	void initialize();

	MemoryBlock *mBackingBlock;
	CellRefRange *mActiveHead;
	CellRefRange *mFreeHead;
};

}
}

#endif

