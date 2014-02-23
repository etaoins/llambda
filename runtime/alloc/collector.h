#ifndef _LLIBY_ALLOC_COLLECTOR_H
#define _LLIBY_ALLOC_COLLECTOR_H

namespace lliby
{
class World;

namespace alloc
{

void* collect(World &world, void *fromBase, void *fromEnd, void *toBase);

}
}

#endif
