#include "binding/BytevectorCell.h"

#include "util/rangeAssertions.h"
#include "core/error.h"

extern "C"
{

using namespace lliby;

// This creates a bytevector cell without initialising its elements
BytevectorCell* llcore_bytevector_alloc(World &world, std::int64_t length)
{
	assertLengthValid(world, "(bytevector)", "bytevector length", BytevectorCell::maximumLength(), length);

	SharedByteArray *newByteArray = SharedByteArray::createUninitialised(length);

	if (newByteArray == nullptr)
	{
		signalError(world, ErrorCategory::OutOfMemory, "Out of memory in (bytevector)");
	}

	return BytevectorCell::withByteArray(world, newByteArray, length);
}

BytevectorCell* llcore_bytevector_alloc_filled(World &world, std::int64_t length, std::uint8_t fill)
{
	assertLengthValid(world, "(make-bytevector)", "bytevector length", BytevectorCell::maximumLength(), length);

	auto bytevector = BytevectorCell::fromFill(world, length, fill);

	if (bytevector == nullptr)
	{
		signalError(world, ErrorCategory::OutOfMemory, "Out of memory in (make-bytevector)");
	}

	return bytevector;
}

}
