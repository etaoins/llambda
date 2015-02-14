#include "MemoryBlock.h"

#include <sys/mman.h>
#include "platform/memory.h"

namespace lliby
{
namespace alloc
{

#ifdef _LLIBY_NO_ADDR_REUSE

MemoryBlock::MemoryBlock(std::size_t size) :
	m_size(size)
{
	m_startPointer = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANON, -1, 0);

	if (m_startPointer == MAP_FAILED)
	{
		m_startPointer = nullptr;
	}
}

MemoryBlock::~MemoryBlock()
{
	if (m_startPointer != nullptr)
	{
		// Mark the old semispace as unreadable but keep the address space allocated
		mprotect(m_startPointer, m_size, PROT_NONE);
	}
}
#else

MemoryBlock::MemoryBlock(std::size_t size)
{
	// Use calloc() because we depend on memory blocks being zero initialised
	m_startPointer = calloc(1, size);
	m_size = platform::mallocActualSize(m_startPointer, size);
}

MemoryBlock::~MemoryBlock()
{
	free(m_startPointer);
}

#endif

}
}
