#ifndef _LLIBY_ALLOC_GARBAGESTATE_H
#define _LLIBY_ALLOC_GARBAGESTATE_H

#include <cstdint>

namespace lliby
{

enum class GarbageState : std::uint8_t
{
	GlobalConstant = 0,
	StackAllocation = 1,
	ColourOne = 2,
	ColourTwo = 3,
	Free = 4
};

}

#endif

