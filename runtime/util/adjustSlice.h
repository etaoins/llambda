#ifndef _LLIBY_UTIL_ADJUSTSLICE_H
#define _LLIBY_UTIL_ADJUSTSLICE_H

namespace
{

/**
 * This ensures that the passed slice is valid and replaces an end index of -1 with the last index
 *
 * This should be used by low-level code before using a slice. Scheme bindings should use assertSliceValid instead to
 * raise a descriptive Scheme error if an incorrect slice is provided
 *
 * @param  start   Provided start index of the slice
 * @param  end     Provided end index of the slice. This this is the special value of -1 this will be updated to equal
 *                 (length - 1)
 * @param  length  Length of the object being sliced
 * @return True if the slice is entirely within the length of object, false otherwise
 *
 */
template<typename T, typename U>
bool adjustSlice(T start, T &end, U length)
{
	if (end == -1)
	{
		end = length;
	}
	else if (end > length)
	{
		return false;
	}

	if (start > end)
	{
		return false;
	}

	return (start >= 0);
}

}

#endif
