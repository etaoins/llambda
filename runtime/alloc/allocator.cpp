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

void freeCons(void *cons)
{
	free(cons);
}

}
}
