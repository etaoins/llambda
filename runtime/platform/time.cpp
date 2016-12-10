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

double utcEpochSeconds()
{
	struct timespec ts;

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

	return static_cast<double>(ts.tv_sec) + (static_cast<double>(ts.tv_nsec) / NanosInSecond);
}

}
}
