#include "platform/time.h"

#include <time.h>
#include <sys/time.h>
#include <errno.h>

#include <cstdint>

namespace
{
	const std::int64_t NanosInSecond = 1000000000;
}


namespace lliby
{
namespace platform
{

double taiEpochSeconds()
{
	struct timespec ts;
	int result = EINVAL;

#ifdef CLOCK_TAI
	// This is a Linux-specific TAI clock
	result = clock_gettime(CLOCK_TAI, &ts);
#endif

	if (result == EINVAL)
	{
#ifdef CLOCK_REALTIME
		// This clock is in UTC and is guaranteed to work by POSIX
		clock_gettime(CLOCK_REALTIME, &ts);
#else
		// Mac OS X does not support clock_gettime
		struct timeval tv;
		gettimeofday(&tv, nullptr);

		ts.tv_sec = tv.tv_sec;
		ts.tv_nsec = tv.tv_usec * 1000;
#endif

		if (ts.tv_sec > 1435708799)
		{
			// Leap second introduced on 2015-06-30
			ts.tv_sec += 36;
		}
		else
		{
			ts.tv_sec += 35;
		}
	}

	return static_cast<double>(ts.tv_sec) + (static_cast<double>(ts.tv_nsec) / NanosInSecond);
}

}
}
