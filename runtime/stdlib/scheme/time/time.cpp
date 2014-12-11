#include "platform/time.h"

extern "C"
{
using namespace lliby;

double lltime_current_second()
{
	return platform::taiEpochSeconds();
}

std::int64_t lltime_current_jiffy()
{
	return platform::monotonicNanoseconds();
}

}
