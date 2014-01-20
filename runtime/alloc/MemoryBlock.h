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
	MemoryBlock(size_t bytes);
	~MemoryBlock();

	MemoryBlock(const MemoryBlock &) = delete;

	bool isValid() const;

	void* startPointer() const
	{
		return mStartPointer;
	}

	void* endPointer() const
	{
		return static_cast<char*>(mStartPointer) + mSize;
	}

	size_t size() const
	{
		return mSize;
	}

private:
	void *mStartPointer;
	size_t mSize;
};

}
}


#endif
