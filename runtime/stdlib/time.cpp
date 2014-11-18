#include "platform/time.h"

extern "C"
{
using namespace lliby;

double lliby_current_second()
{
	return platform::taiEpochSeconds();
}

std::int64_t lliby_current_jiffy()
{
	return platform::monotonicNanoseconds();
}

}
