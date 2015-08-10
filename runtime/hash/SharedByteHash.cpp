#include "hash/SharedByteHash.h"

namespace
{
	const SharedByteHash::ResultType ImpossibleRemapValue = 0x86b2bb0d;
}

SharedByteHash::ResultType SharedByteHash::operator()(const std::uint8_t *data, std::size_t size)
{
	std::uint32_t h = 5381;
	for(std::size_t i = 0; i < size; i++)
	{
		h = (h * 33) + data[i];
	}

	if (h == ImpossibleResultValue)
	{
		return ImpossibleRemapValue;
	}
	else
	{
		return h;
	}
}

