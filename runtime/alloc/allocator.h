#ifndef _LLIBY_ALLOC_ALLOCATOR_H
#define _LLIBY_ALLOC_ALLOCATOR_H

#include <stddef.h>

namespace lliby
{
class World;

namespace alloc
{

class RangeAlloc;

void initGlobal();

void initWorld(World &world);
void shutdownWorld(World &world);

/**
 * Allocator for DatumCells
 */
void *allocateCells(World &, size_t count = 1);
RangeAlloc allocateRange(World &, size_t count);

/**
 * Forces a major GC collection
 */
bool forceCollection(World &world, size_t reserveCount = 0);

}
}

#endif

