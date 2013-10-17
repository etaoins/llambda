#include "alloc/allocator.h"
#include "alloc/Cons.h"

#include <stdlib.h>

namespace lliby
{
namespace alloc
{
    
void *allocateCons(size_t count)
{
	return malloc(sizeof(Cons) * count);
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
