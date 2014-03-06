#include "platform/memory.h"

#include <stdlib.h>

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

SizedMallocResult sizedMalloc(size_t minimumSize)
{
	void *basePointer = malloc(minimumSize);

#ifdef __APPLE__
	return SizedMallocResult {
		.basePointer = basePointer,
		.actualSize = malloc_size(basePointer)
	};
#elif defined(__GNU_LIBRARY__) || defined(__FreeBSD__)
	return SizedMallocResult {
		.basePointer = basePointer,
		.actualSize = malloc_usable_size(basePointer)
	};
#else
	return SizedMallocResult {
		.basePointer = basePointer,
		.actualSize = minimumSize

	};
#endif
}

}
}
