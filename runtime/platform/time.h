#ifndef _LLIBY_PLATFORM_TIME_H
#define _LLIBY_PLATFORM_TIME_H

#include <cstdint>

namespace lliby
{
namespace platform
{

/**
 * Returns the number of seconds since January 1st, 1970 in International Atomic Time
 *
 * This corresponds to (current-second) in R7RS
 */
double taiEpochSeconds();

/**
 * Returns the number of seconds since January 1st, 1970 in UTC
 */
double utcEpochSeconds();

}
}

#endif
