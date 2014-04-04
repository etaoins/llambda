#ifndef _LLIBY_ALLOC_SHADOWSTACKENTRY_H
#define _LLIBY_ALLOC_SHADOWSTACKENTRY_H

#include <cstdint>

#include "binding/DatumCell.h"

namespace lliby
{
namespace alloc
{

/**
 * Represents an entry on the shadow stack generated by the GC
 */
struct ShadowStackEntry
{
	ShadowStackEntry *next;
	std::uint64_t cellCount;
	DatumCell* roots[];
};

}
}

#endif