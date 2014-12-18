#ifndef _LLIBY_UTIL_ASSERTINDEXVALID_H
#define _LLIBY_UTIL_ASSERTINDEXVALID_H

#include "core/error.h"

#include <sstream>
#include <cstdint>

namespace lliby
{

/** Validates that an index is valid for a given object length
 *
 * If the index is valid this function will return normally; otherwise, an error will be signalled in the passed world.
 *
 * @param  world        World to signal an error in if the slice is invalid
 * @param  procName     Scheme name of the calling procedure. This is used in the signalled error message.
 * @param  obj          Object having its slice validated. This is used as the evidence if an error is signalled.
 * @param  objLength    Length of the object in the same units as "index"
 * @param  index        User supplied index
 */
template<typename T>
void assertIndexValid(World &world, const char *procName, AnyCell *obj, T objLength, std::int64_t index)
{
	if (index >= objLength)
	{
		std::ostringstream message;
		message << "Index of " << index << " is past length of " << objLength << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str().c_str(), {obj});
	}

	if (index < 0)
	{
		std::ostringstream message;
		message << "Negative index of " << index << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str().c_str(), {obj});
	}
}

}

#endif
