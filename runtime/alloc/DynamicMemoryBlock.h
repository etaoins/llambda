#ifndef _LLIBY_ALLOC_DYNAMICMEMORYBLOCK_H
#define _LLIBY_ALLOC_DYNAMICMEMORYBLOCK_H

#include "alloc/MemoryBlock.h"

namespace lliby
{
namespace alloc
{

class DynamicMemoryBlock : public MemoryBlock
{
public:
	DynamicMemoryBlock(size_t size);
	~DynamicMemoryBlock();

	void* startPointer() const
	{
		return mStartPointer;
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
