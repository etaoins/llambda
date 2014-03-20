#ifndef _LLIBY_ALLOC_COLLECTOR_H
#define _LLIBY_ALLOC_COLLECTOR_H

#include <cstddef>

namespace lliby
{
class World;
class Heap;

namespace alloc
{

size_t collect(World &world, Heap &newHeap);

}
}

#endif
