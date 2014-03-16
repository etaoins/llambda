#ifndef _LLIBY_ALLOC_COLLECTOR_H
#define _LLIBY_ALLOC_COLLECTOR_H

namespace lliby
{
class World;
class Heap;

namespace alloc
{

void collect(World &world, Heap &newHeap);

}
}

#endif
