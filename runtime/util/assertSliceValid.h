#ifndef _LLIBY_UTIL_ASSERTSLICEVALID_H
#define _LLIBY_UTIL_ASSERTSLICEVALID_H

#include "core/error.h"

#include <sstream>

namespace lliby
{

/** Validates that slice indices are valid for a given object length
 *
 * If the slice is valid this function will return normally; otherwise, an error will be signalled in the passed world.
 *
 * @param  world        World to signal an error in if the slice is invalid
 * @param  procName     Scheme name of the calling procedure. This is used in the signalled error message.
 * @param  obj          Object having its slice validated. This is used as the evidence if an error is signalled.
 * @param  objLength    Length of the object in the same units as "start" and "end"
 * @param  start        User supplied start index
 * @param  end          User supplied end index
 */
template<typename T>
void assertSliceValid(World &world, const char *procName, AnyCell *obj, T objLength, T start, T end)
{
	if (end > objLength)
	{
		std::ostringstream message;
		message << "Slice end index of " << end << " is past length of " << objLength << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str().c_str(), {obj});
	}

	if (start > end)
	{
		std::ostringstream message;
		message << "Slice start index of " << start << " is greater than end index of " << end << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str().c_str(), {obj});
	}
}

}

#endif
