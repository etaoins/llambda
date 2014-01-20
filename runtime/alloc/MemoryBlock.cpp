#include "MemoryBlock.h"

#include <sys/mman.h>

namespace lliby
{
namespace alloc
{
	
MemoryBlock::MemoryBlock(size_t bytes) :
	mSize(bytes)
{
	mStartPointer = mmap(NULL, bytes, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);
}

MemoryBlock::~MemoryBlock()
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

bool MemoryBlock::isValid() const
{
	return mStartPointer != MAP_FAILED;
}

}
}
