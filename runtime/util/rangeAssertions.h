#ifndef _LLIBY_UTIL_RANGEASSERTIONS_H
#define _LLIBY_UTIL_RANGEASSERTIONS_H

#include <cstdint>

#include "core/World.h"

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
void assertIndexValid(World &world, const char *procName, AnyCell *obj, std::int64_t objLength, std::int64_t index);

/** Validates that a length is valid for a given object type
 *
 * If the length is valid this function will return normally; otherwise, an error will be signalled in the passed world.
 *
 * @param  world        World to signal an error in if the slice is invalid
 * @param  procName     Scheme name of the calling procedure. This is used in the signalled error message.
 * @param  lengthName   Human readable description of the length being tested - e.g. "vector length" or "string byte
 *                      length"
 * @param  maxLength    Maximum length of the object type in the same units as "length"
 * @param  length       User supplied length
 */
void assertLengthValid(World &world, const char *procName, const char *lengthName, std::int64_t maxLength, std::int64_t length);

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
void assertSliceValid(World &world, const char *procName, AnyCell *obj, std::int64_t objLength, std::int64_t start, std::int64_t end);

}

#endif
