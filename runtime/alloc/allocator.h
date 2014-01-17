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
 * Allocator for DatumCells
 */
void *allocateCells(size_t count = 1);
RangeAlloc allocateRange(size_t count);

}
}

#endif

