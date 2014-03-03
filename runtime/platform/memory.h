#ifndef _LLIBY_PLATFORM_ALLOCCELL_H
#define _LLIBY_PLATFORM_ALLOCCELL_H

#include <stdlib.h>

namespace lliby
{
namespace platform
{

struct SizedMallocResult
{
	void *basePointer;
	size_t actualSize;
};

SizedMallocResult sizedMalloc(size_t minimumSize);

}
}

#endif
