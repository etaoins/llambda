#ifndef _LLIBY_HASH_DATUMHASH_H
#define _LLIBY_HASH_DATUMHASH_H

#include <cstdint>

#include "binding/AnyCell.h"

namespace lliby
{

/** Function object for calculatng the hash of AnyCell objects for (equals?) equality
 *
 * This can be used directly as a Hash function for STL containers
 */
struct DatumHash
{
	using ResultType = std::uint32_t;

 	ResultType operator()(AnyCell *) const;
};

}

#endif
