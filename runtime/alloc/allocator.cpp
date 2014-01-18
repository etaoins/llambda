#include "alloc/allocator.h"

#include <stdlib.h>
#include <iostream>
#include <stdio.h>
#include <sys/mman.h>

#include "alloc/AllocCell.h"
#include "alloc/RangeAlloc.h"
#include "alloc/collector.h"

// Statically check that everything can fit in to a cell
#include "generated/sizecheck.h"

extern "C"
{

using lliby::alloc::AllocCell;

// These are used directly by generated code to avoid function call overhead

// Pointer to the next cell to be allocated
AllocCell *_lliby_alloc_next = nullptr;
// Pointer to the end of the last available allocation cell
AllocCell *_lliby_alloc_end = nullptr;

auto _lliby_alloc_cells = lliby::alloc::allocateCells;

}

namespace
{
	const size_t SemiSpaceSize = 4 * 1024 * 1024; 

	// Pointer to the start of the semi-space
	void *semiSpaceStart = nullptr;
}

namespace lliby
{
namespace alloc
{

void init()
{
	// This will create our initial semispace
	forceCollection();
}
    
void *allocateCells(size_t count)
{
	AllocCell *allocation = _lliby_alloc_next;
	_lliby_alloc_next += count;

	if (_lliby_alloc_next > _lliby_alloc_end)
	{
		if (!forceCollection(count))
		{
			std::cerr << "GC space exhausted" << std::endl;
			exit(-2);
		}
	
		// If forceCollection() returned true this must succeed
		allocation = _lliby_alloc_next;
		_lliby_alloc_next += count;
	}

	// Mark the cells as allocated
	for(size_t i = 0; i < count; i++)
	{
		allocation[i].setGcState(GarbageState::AllocatedCell);
	}

	return allocation;
}

RangeAlloc allocateRange(size_t count)
{
	auto start = static_cast<AllocCell*>(allocateCells(count));
	auto end = start + count;

	return RangeAlloc(start, end);
}

bool forceCollection(size_t reserveCount)
{
	// Directly allocate memory from the kernel
	void *oldSemiSpaceStart = semiSpaceStart;
	void *newSemiSpaceStart = mmap(NULL, SemiSpaceSize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);

	if (newSemiSpaceStart == MAP_FAILED)
	{
		perror("mmap");
		std::cerr << "Unable to allocate " << SemiSpaceSize << " bytes" << std::endl;
		exit(-2);
	}

	if (oldSemiSpaceStart != nullptr)
	{
		// Garbage collect if this isn't our first allocation
		_lliby_alloc_next = static_cast<AllocCell*>(alloc::collect(oldSemiSpaceStart, _lliby_alloc_end, newSemiSpaceStart));

		// Free the old semispace
		munmap(oldSemiSpaceStart, SemiSpaceSize);
	}
	else 
	{
		// No previous allocation; we can start at the beginning of the semispace
		_lliby_alloc_next = static_cast<AllocCell*>(newSemiSpaceStart);
	}

	// Set up the new pointers
	auto semiSpaceEnd = reinterpret_cast<AllocCell*>(static_cast<char*>(newSemiSpaceStart) + SemiSpaceSize); 
	semiSpaceStart = newSemiSpaceStart;

#ifdef _LLIBY_ALWAYS_GC
	// This will trigger the GC again on the next allocation. This will break GC unsafe code at every allocation point
	// which is useful for shaking out bugs.
	_lliby_alloc_end = _lliby_alloc_next + reserveCount;

	return _lliby_alloc_end <= semiSpaceEnd;
#else	
	_lliby_alloc_end = semiSpaceEnd;
	
	// Make sure the reserved space will fit 
	return (_lliby_alloc_next + reserveCount) <= _lliby_alloc_end;
#endif
	

}

}
}
