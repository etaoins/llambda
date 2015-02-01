#ifndef _LLIBY_ALLOC_ALLOCATOR_H
#define _LLIBY_ALLOC_ALLOCATOR_H

#include <cstddef>

namespace lliby
{
class World;

namespace alloc
{

class AllocCell;
class RangeAlloc;

void initGlobal();
void shutdownGlobal();

/**
 * Allocator for AnyCells
 */
AllocCell *allocateCells(World &, size_t count = 1);
RangeAlloc allocateRange(World &, size_t count);

/**
 * Forces a major GC collection returning the number of reachable cells
 */
size_t forceCollection(World &world);

}
}

#endif

