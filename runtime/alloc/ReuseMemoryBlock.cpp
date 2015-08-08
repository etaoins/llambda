#include "MemoryBlock.h"

#include <iostream>
#include <cstdlib>

#include "platform/memory.h"

namespace lliby
{
namespace alloc
{

MemoryBlock* MemoryBlock::create(std::size_t size)
{
	MemoryBlock *newAlloc = static_cast<MemoryBlock*>(malloc(size));

	if (newAlloc == nullptr)
	{
		std::cerr << "Unable to allocate " << size << " bytes" << std::endl;
		exit(-2);
	}

	return newAlloc;
}

std::size_t MemoryBlock::size(std::size_t requestedSize) const
{
	return platform::mallocActualSize(const_cast<MemoryBlock*>(this), requestedSize);
}

}
}
