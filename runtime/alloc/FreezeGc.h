#ifndef _LLIBY_ALLOC_FREEZEGC_H
#define _LLIBY_ALLOC_FREEZEGC_H

#include "allocator.h"

namespace lliby
{
namespace alloc
{

extern unsigned int FreezeCount;

class FreezeGc
{
public:
	explicit FreezeGc(size_t reservedCons = 0)
	{
		if (reservedCons)
		{
			preallocCons(reservedCons);
		}

		FreezeCount++;
	}

	~FreezeGc()
	{
		FreezeCount--;
	}
};

}
}

#endif

