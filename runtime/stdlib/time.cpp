#include <time.h>
#include <sys/time.h>
#include <errno.h>

#include <cstdint>

namespace
{
	const std::int64_t NanosInSecond = 1000000000;

	void fetch_realtime(struct timespec *ts)
	{
#ifdef CLOCK_REALTIME
		// This clock is in UTC and is guaranteed to work by POSIX
		clock_gettime(CLOCK_REALTIME, &ts);
#else
		// Mac OS X does not support clock_gettime
		struct timeval tv;
		gettimeofday(&tv, nullptr);

		ts->tv_sec = tv.tv_sec;
		ts->tv_nsec = tv.tv_usec * 1000;
#endif
	}
}

extern "C"
{

double lliby_current_second()
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

std::int64_t lliby_current_jiffy()
{
	struct timespec ts;
	int result = EINVAL;

#ifdef CLOCK_MONOTONIC
	result = clock_gettime(CLOCK_MONOTONIC, &ts);
#endif

	if (result == EINVAL)
	{
		fetch_realtime(&ts);
	}

	return (static_cast<std::int64_t>(ts.tv_sec) * NanosInSecond) + ts.tv_nsec;
}

}
