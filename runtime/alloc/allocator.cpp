#include "alloc/allocator.h"
#include "alloc/Cell.h"
#include "alloc/RangeAlloc.h"

#include <stdlib.h>
#include <iostream>
#include <stdio.h>
#include <sys/mman.h>

// Statically check that everything can fit in to a cell
#include "generated/sizecheck.h"

extern "C"
{

using lliby::alloc::Cell;

// These are used directly by generated code to avoid function call overhead
Cell *_lliby_alloc_start = nullptr;
Cell *_lliby_alloc_end = nullptr;

auto _lliby_alloc_cells = lliby::alloc::allocateCells;

}

namespace
{
	const size_t SemiSpaceSize = 4 * 1024 * 1024; 
}

namespace lliby
{
namespace alloc
{

void init()
{
	// Directly allocate memory from the kernel
	void *mmappedMemory = mmap(NULL, SemiSpaceSize, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);

	if (mmappedMemory == MAP_FAILED)
	{
		perror("mmap");
		std::cerr << "Unable to allocate " << SemiSpaceSize << " bytes" << std::endl;
		exit(-2);
	}

	_lliby_alloc_start = static_cast<Cell*>(mmappedMemory);
	_lliby_alloc_end = reinterpret_cast<Cell*>(static_cast<char*>(mmappedMemory) + SemiSpaceSize);
}
    
void *allocateCells(size_t count)
{
	Cell *allocation = _lliby_alloc_start;

	_lliby_alloc_start += count;

	if (_lliby_alloc_start > _lliby_alloc_end) 
	{
		std::cerr << "Semispace exhausted" << std::endl;
		exit(-2);
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
	auto start = static_cast<Cell*>(allocateCells(count));
	auto end = start + count;

	return RangeAlloc(start, end);
}

}
}
