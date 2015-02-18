#include <cstddef>
#include <cstdlib>

namespace lliby
{
namespace alloc
{

class MemoryBlock
{
public:
	static MemoryBlock* create(std::size_t size);

	void operator delete(void *p)
	{
		free(p);
	}

	void* startPointer() const
	{
		return const_cast<MemoryBlock*>(this);
	}

	std::size_t size(std::size_t requestedSize) const;
};

}
}
