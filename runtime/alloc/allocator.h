#ifndef _LLIBY_ALLOC_ALLOCATOR_H
#define _LLIBY_ALLOC_ALLOCATOR_H

#include <cstddef>

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
 * Forces a major GC collection returning the number of reachable cells
 */
size_t forceCollection(World &world);

}
}

#endif

