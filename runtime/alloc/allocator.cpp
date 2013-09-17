#include "alloc/allocator.h"
#include "alloc/Cons.h"

#include <stdlib.h>

namespace lliby
{
namespace alloc
{
    
void *allocateCons()
{
	return malloc(sizeof(Cons));
}

void preallocCons(size_t count)
{
}

void freeCons(void *cons)
{
	free(cons);
}

}
}
