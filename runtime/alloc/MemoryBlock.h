#ifndef _LLIBY_ALLOC_MEMORYBLOCK_H
#define _LLIBY_ALLOC_MEMORYBLOCK_H

#ifdef _LLIBY_NO_ADDR_REUSE
	#include "NoReuseMemoryBlock.h"
#else
	#include "ReuseMemoryBlock.h"
#endif

#endif
