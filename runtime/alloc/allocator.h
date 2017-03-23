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

void reportGlobalLeaks();

/**
 * Allocator for AnyCells
 */
AllocCell *allocateCells(World &, std::size_t count = 1);
RangeAlloc allocateRange(World &, std::size_t count);

/**
 * Forces a major GC collection returning the number of reachable cells
 */
std::size_t forceCollection(World &world);

}
}

#endif

