#include <cstddef>

namespace lliby
{
namespace alloc
{

class MemoryBlock
{
public:
	static MemoryBlock* create(std::size_t size);
	~MemoryBlock();

	void* startPointer() const
	{
		return m_startPointer;
	}

	std::size_t size(std::size_t) const
	{
		return m_size;
	}

private:
	void *m_startPointer;
	std::size_t m_size;
};

}
}
