#include "core/error.h"

#include <sstream>
#include <cstdint>

namespace lliby
{

void assertIndexValid(World &world, const char *procName, AnyCell *obj, std::int64_t objLength, std::int64_t index)
{
	if (index >= objLength)
	{
		std::ostringstream message;
		message << "Index of " << index << " is past length of " << objLength << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str(), {obj});
	}

	if (index < 0)
	{
		std::ostringstream message;
		message << "Negative index of " << index << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str(), {obj});
	}
}

void assertLengthValid(World &world, const char *procName, const char *lengthName, std::int64_t maxLength, std::int64_t length)
{
	if (length > maxLength)
	{
		std::ostringstream message;
		message << "Length of " << length << " exceeds maximum " << lengthName << " of " << maxLength << " in " << procName;

		signalError(world, ErrorCategory::ImplementationRestriction, message.str());
	}

	if (length < 0)
	{
		std::ostringstream message;
		message << "Negative " << lengthName << " of " << length << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str());
	}
}

void assertSliceValid(World &world, const char *procName, AnyCell *obj, std::int64_t objLength, std::int64_t start, std::int64_t end)
{
	if (end > objLength)
	{
		std::ostringstream message;
		message << "Slice end index of " << end << " is past length of " << objLength << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str(), {obj});
	}

	if (start < 0)
	{
		std::ostringstream message;
		message << "Negative slice start index of " << start << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str(), {obj});
	}
	else if (start > end)
	{
		std::ostringstream message;
		message << "Slice start index of " << start << " is greater than end index of " << end << " in " << procName;

		signalError(world, ErrorCategory::Range, message.str(), {obj});
	}
}

}
