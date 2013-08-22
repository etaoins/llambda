#ifndef _LLIBY_ALLOC_ALLOCATOR_H
#define _LLIBY_ALLOC_ALLOCATOR_H

namespace lliby
{
namespace alloc
{

/**
 * Allocator for BoxedDatums
 */
void *allocateCons();

void freeCons(void *);
	
}
}

#endif

