#include "MemoryBlock.h"

#include <iostream>
#include <cstdlib>

#include <sys/mman.h>

namespace lliby
{
namespace alloc
{

MemoryBlock *MemoryBlock::create(std::size_t size)
{
	MemoryBlock *newBlock = new MemoryBlock;

	newBlock->m_size = size;
	newBlock->m_startPointer = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);

	if (newBlock->m_startPointer == MAP_FAILED)
	{
		std::cerr << "Unable to allocate " << size << " bytes" << std::endl;
		exit(-2);
	}

	return newBlock;
}

MemoryBlock::~MemoryBlock()
{
	if (m_startPointer != nullptr)
	{
		// Mark the old semispace as unreadable but keep the address space allocated
		mprotect(m_startPointer, m_size, PROT_NONE);
	}
}

}
}
