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

	static void init();

	void* startPointer() const
	{
		return m_startPointer;
	}

	size_t size() const
	{
		return m_size;
	}

private:
	void *m_startPointer;
	size_t m_size;
};

}
}


#endif
