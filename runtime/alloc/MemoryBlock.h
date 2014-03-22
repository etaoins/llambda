#ifndef _LLIBY_ALLOC_MEMORYBLOCK_H
#define _LLIBY_ALLOC_MEMORYBLOCK_H

#include <cstddef>

namespace lliby
{
namespace alloc
{

class MemoryBlock
{
public:
	MemoryBlock()
	{
	}

	MemoryBlock(const MemoryBlock &) = delete;

	virtual ~MemoryBlock()
	{
	}

	bool isValid() const
	{
		return startPointer() != nullptr;
	}

	virtual void* startPointer() const = 0;
	virtual size_t size() const = 0;

	void* endPointer() const
	{
		return static_cast<char*>(startPointer()) + size();
	}
};

}
}

#endif
