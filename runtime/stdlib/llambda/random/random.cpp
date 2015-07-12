#include "core/World.h"
#include "core/error.h"

#include <random>

extern "C"
{
using namespace lliby;

std::int64_t llrandom_random_integer(World &world, std::int64_t n)
{
	if (n < 1)
	{
		signalError(world, ErrorCategory::Range, "Argument to (random-integer) must be a positive integer");
	}

	std::random_device rd;
	std::mt19937 mt(rd());
	std::uniform_int_distribution<std::int64_t> dis(0, n - 1);

	return dis(mt);
}

double llrandom_random_real()
{
	std::random_device rd;
	std::mt19937 mt(rd());
	std::uniform_real_distribution<double> dis(std::numeric_limits<double>::min(), 1.0);

	return dis(mt);
}

}
