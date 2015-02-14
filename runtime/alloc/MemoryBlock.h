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
	MemoryBlock(std::size_t size);
	~MemoryBlock();

	void* startPointer() const
	{
		return m_startPointer;
	}

	std::size_t size() const
	{
		return m_size;
	}

	bool isValid() const
	{
		return m_startPointer != nullptr;
	}

private:
	void *m_startPointer;
	std::size_t m_size;
};

}
}

#endif
