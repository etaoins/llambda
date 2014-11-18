#ifndef _LLIBY_PLATFORM_ALLOCCELL_H
#define _LLIBY_PLATFORM_ALLOCCELL_H

#include <cstdlib>

namespace lliby
{
namespace platform
{

/**
 * Returns the usable size of malloc() allocated memory
 *
 * @param  allocation    Allocation to query the size of
 * @param  fallbackSize  Size to return if the query fails
 */
std::size_t mallocActualSize(void *allocation, std::size_t fallbackSize);

}
}

#endif
