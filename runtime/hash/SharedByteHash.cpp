#include "hash/SharedByteHash.h"

namespace
{
	const SharedByteHash::ResultType ImpossibleRemapValue = 0x86b2bb0d;
	const std::uint32_t FNV1APrime = 0x1000193;
	const std::uint32_t FNV1AOffsetBasis = 0x811C9DC5;
}

SharedByteHash::ResultType SharedByteHash::operator()(const std::uint8_t *data, std::size_t size)
{
	std::uint32_t h = FNV1AOffsetBasis;
	for(std::size_t i = 0; i < size; i++)
	{
		h ^= data[i];
		h *= FNV1APrime;
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

