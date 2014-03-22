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
		madvise(mStartPointer, mSize, MADV_DONTNEED);
		mprotect(mStartPointer, mSize, PROT_NONE);
#endif
	}
}

}
}
