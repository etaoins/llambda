#ifndef _LLIBY_ALLOC_ALLOCATOR_H
#define _LLIBY_ALLOC_ALLOCATOR_H

#include <stddef.h>

namespace lliby
{
namespace alloc
{

class RangeAlloc;

void init();

/**
 * Allocator for BoxedDatums
 */
void *allocateCons(size_t count = 1);
RangeAlloc allocateRange(size_t count);

void preallocCons(size_t count);
	
}
}

#endif

