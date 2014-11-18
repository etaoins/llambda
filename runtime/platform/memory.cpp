#include "platform/memory.h"

#ifdef __APPLE__
#include <malloc/malloc.h>
#elif defined(__GNU_LIBRARY__)
#include <malloc.h>
#elif defined(__FreeBSD__)
#include <malloc_np.h>
#endif

namespace lliby
{
namespace platform
{

std::size_t mallocActualSize(void *allocation, std::size_t fallbackSize)
{
#ifdef __APPLE__
	return malloc_size(allocation);
#elif defined(__GNU_LIBRARY__) || defined(__FreeBSD__)
	return malloc_usable_size(allocation);
#else
	return fallbackSize;
#endif
}

}
}
