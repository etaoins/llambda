#ifndef _LLIBY_ALLOC_COLLECTOR_H
#define _LLIBY_ALLOC_COLLECTOR_H

namespace lliby
{
namespace alloc
{

void* collect(void *fromBase, void *fromEnd, void *toBase);

}
}

#endif
