#include "DynamicMemoryBlock.h"

#include <sys/mman.h>

namespace lliby
{
namespace alloc
{

DynamicMemoryBlock::DynamicMemoryBlock(size_t size) :
	MemoryBlock(),
	m_size(size)
{
	m_startPointer = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);

	if (m_startPointer == MAP_FAILED)
	{
		m_startPointer = nullptr;
	}
}

DynamicMemoryBlock::~DynamicMemoryBlock()
{
	if (m_startPointer != nullptr)
	{
#ifndef _LLIBY_NO_ADDR_REUSE
		// Free the old semispace
		munmap(m_startPointer, m_size);
#else
		// Mark the old semispace as unreadable but keep the address space allocated
		mprotect(m_startPointer, m_size, PROT_NONE);
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
