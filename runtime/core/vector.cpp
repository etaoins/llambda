#include "binding/VectorCell.h"

#include "util/rangeAssertions.h"
#include "core/error.h"

extern "C"
{

using namespace lliby;

// This creates a vector cell without initialising its elements
VectorCell* llcore_vector_alloc(World &world, std::int64_t length)
{
	assertLengthValid(world, "(make-vector)", "vector length", VectorCell::maximumLength(), length);

	AnyCell **elements;

	try
	{
		elements = new AnyCell*[length];
	}
	catch(std::bad_alloc &)
	{
		signalError(world, ErrorCategory::OutOfMemory, "Out of memory while constructing vector");
	}

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) VectorCell(elements, length);
}

}
