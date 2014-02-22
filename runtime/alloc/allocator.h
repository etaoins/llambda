#ifndef _LLIBY_ALLOC_ALLOCATOR_H
#define _LLIBY_ALLOC_ALLOCATOR_H

#include <stddef.h>

namespace lliby
{
class World;

namespace alloc
{

class RangeAlloc;

void init();
void shutdown();

/**
 * Allocator for DatumCells
 */
void *allocateCells(size_t count = 1);
RangeAlloc allocateRange(World &, size_t count);

/**
 * Forces a major GC collection
 */
bool forceCollection(size_t reserveCount = 0);

}
}

#endif

