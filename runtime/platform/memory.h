#ifndef _LLIBY_PLATFORM_ALLOCCELL_H
#define _LLIBY_PLATFORM_ALLOCCELL_H

#include <cstdlib>

namespace lliby
{
namespace platform
{

struct SizedMallocResult
{
	/**
	 * Base pointer of the sized malloc
	 *
	 * This must be freed using free()
	 */
	void *basePointer;

	/**
	 * Actual usable size of the allocation in bytes
	 *
	 * This is guaranteed to be at least as large as the minimumSize parameter passed to sizedMalloc()
	 */
	size_t actualSize;
};

/**
 * Allocates a minimum number of bytes and returns the actual allocation size
 *
 * There is currently no platform-independent way to accomplish this. Unsupported platforms fall back to a direct 
 * malloc().
 *
 * @param  minimumBytes  Minimum size in bytes of the allocation
 */
SizedMallocResult sizedMalloc(size_t minimumSize);

}
}

#endif
