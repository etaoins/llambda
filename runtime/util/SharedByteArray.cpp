#include "util/SharedByteArray.h"

#ifdef _LLIBY_CHECK_LEAKS

namespace
{
	std::atomic<size_t> allocationCount(0);
}

namespace lliby
{
	
SharedByteArray::~SharedByteArray()
{
	allocationCount.fetch_sub(1, std::memory_order_relaxed);
}

void SharedByteArray::incrementInstanceCount()
{
	allocationCount.fetch_add(1, std::memory_order_relaxed);
}

size_t SharedByteArray::instanceCount()
{
	return allocationCount.load(std::memory_order_relaxed);
}

}

#endif
