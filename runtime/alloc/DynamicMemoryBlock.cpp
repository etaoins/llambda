#include "DynamicMemoryBlock.h"

#include <sys/mman.h>

namespace lliby
{
namespace alloc
{
	
DynamicMemoryBlock::DynamicMemoryBlock(size_t size) :
	MemoryBlock(),
	mSize(size)
{
	mStartPointer = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);

	if (mStartPointer == MAP_FAILED)
	{
		mStartPointer = nullptr;
	}
}

DynamicMemoryBlock::~DynamicMemoryBlock()
{
	if (mStartPointer != nullptr)
	{
#ifndef _LLIBY_NO_ADDR_REUSE
		// Free the old semispace
		munmap(mStartPointer, mSize);
#else
		// Mark the old semispace as unreadable but keep the address space allocated
		mprotect(mStartPointer, mSize, PROT_NONE);
#endif
	}
}
	
void DynamicMemoryBlock::init()
{
#if !defined(NDEBUG) && defined(__APPLE__)
	// XXX: Valgrind 3.10.0 on Mac OS X 10.9 will return a NULL pointer for the first mmap()
	// This confuses DynamicMemoryBlock greatly
	// Create a throwaway allocation so subsequent allocations succeed
	mmap(NULL, 4096, PROT_NONE, MAP_PRIVATE | MAP_ANON, -1, 0);
#endif
}

}
}
