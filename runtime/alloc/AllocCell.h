#ifndef _LLIBY_ALLOC_ALLOCCELL_H
#define _LLIBY_ALLOC_ALLOCCELL_H

#include "binding/PairCell.h"

namespace lliby
{
namespace alloc
{

class MemoryBlock;

// This is a placeholder for size purposes
// We assume a PairCell is the largest allocation with two pointers
// If this isn't true then sizecheck.h will assert at compile time
class AllocCell : public DatumCell
{
protected:
	std::uint8_t padding[22]; 
};

// This is a special cell that terminates a heap segment
class SegmentTerminatorCell : public DatumCell
{
public:
	SegmentTerminatorCell(MemoryBlock *nextSegment) :
		DatumCell(CellTypeId::Invalid, GarbageState::SegmentTerminator),
		m_nextSegment(nextSegment)
	{
	}

	MemoryBlock* nextSegment() const
	{
		return m_nextSegment;
	}

private:
	MemoryBlock *m_nextSegment;
};

// This is a special cell that terminates an entire
class HeapTerminatorCell : public DatumCell
{
public:
	HeapTerminatorCell() :
		DatumCell(CellTypeId::Invalid, GarbageState::HeapTerminator)
	{
	}
};

}
}

#endif

