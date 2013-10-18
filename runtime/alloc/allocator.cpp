#include "alloc/allocator.h"
#include "alloc/Cons.h"

#include <stdlib.h>
#include <iostream>
#include <stdio.h>
#include <sys/mman.h>

extern "C"
{

using lliby::alloc::Cons;

// These are used directly by generated code to avoid function call overhead
Cons *_lliby_alloc_start = nullptr;
Cons *_lliby_alloc_end = nullptr;

auto _lliby_alloc_cons = lliby::alloc::allocateCons;

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

	_lliby_alloc_start = static_cast<Cons*>(mmappedMemory);
	_lliby_alloc_end = reinterpret_cast<Cons*>(static_cast<char*>(mmappedMemory) + SemiSpaceSize);
}
    
void *allocateCons(size_t count)
{
	Cons *allocation = _lliby_alloc_start;

	_lliby_alloc_start += count;

	if (_lliby_alloc_start > _lliby_alloc_end) 
	{
		std::cerr << "Semispace exhausted" << std::endl;
		exit(-2);
	}

	return allocation;
}

void preallocCons(size_t count)
{
}

}
}
