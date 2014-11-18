#include "platform/time.h"

#include <time.h>
#include <sys/time.h>
#include <errno.h>

#include <cstdint>

#ifdef __APPLE__
#include <CoreServices/CoreServices.h>
#include <mach/mach.h>
#include <mach/mach_time.h>
#include <unistd.h>
#endif

namespace
{
	const std::int64_t NanosInSecond = 1000000000;

	void fetch_realtime(struct timespec *ts)
	{
#ifdef CLOCK_REALTIME
		// This clock is in UTC and is guaranteed to work by POSIX
		clock_gettime(CLOCK_REALTIME, ts);
#else
		// Mac OS X does not support clock_gettime
		struct timeval tv;
		gettimeofday(&tv, nullptr);

		ts->tv_sec = tv.tv_sec;
		ts->tv_nsec = tv.tv_usec * 1000;
#endif
	}
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
		fetch_realtime(&ts);

		// The offset from UTC to TAI in 2014-11 is +35 seconds
		ts.tv_sec += 35;
	}

	return static_cast<double>(ts.tv_sec) + (static_cast<double>(ts.tv_nsec) / NanosInSecond);
}

std::int64_t monotonicNanoseconds()
{
#ifdef __APPLE__
	static mach_timebase_info_data_t timebaseInfo;

	std::uint64_t elapsed = mach_absolute_time();

	if (timebaseInfo.denom == 0)
	{
		mach_timebase_info(&timebaseInfo);
	}

	return elapsed * timebaseInfo.numer / timebaseInfo.denom;
#else
	struct timespec ts;
	int result = EINVAL;

#ifdef CLOCK_MONOTONIC
	result = clock_gettime(CLOCK_MONOTONIC, &ts);
#endif // CLOCK_MONOTONIC

	if (result == EINVAL)
	{
		fetch_realtime(&ts);
	}

	return (static_cast<std::int64_t>(ts.tv_sec) * NanosInSecond) + ts.tv_nsec;
#endif // __APPLE__
}

}
}
