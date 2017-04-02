#ifndef _LLIBY_HASH_SHAREDBYTEHASH_H
#define _LLIBY_HASH_SHAREDBYTEHASH_H

#include <cstdint>
#include <cstddef>

struct SharedByteHash
{
	using ResultType = std::uint32_t;
	static const ResultType ImpossibleResultValue = 0;

	/**
	 * Hashes the passed binary data
	 *
	 * This is guaranteed to never return ImpossibleResultValue
	 */
	ResultType operator()(const std::uint8_t *data, std::size_t size);
};

#endif
