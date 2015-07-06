#include "platform/time.h"

#include <chrono>

extern "C"
{
using namespace lliby;

double lltime_current_unix_time()
{
	return platform::utcEpochSeconds();
}

}
