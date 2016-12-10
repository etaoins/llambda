#include "platform/time.h"

#include <chrono>

extern "C"
{
using namespace lliby;

double lltime_current_unix_time()
{
	return platform::utcEpochSeconds();
}

std::int64_t lltime_current_jiffy()
{
	using namespace std::chrono;
	return duration_cast<nanoseconds>(steady_clock::now().time_since_epoch()).count();
}

}
