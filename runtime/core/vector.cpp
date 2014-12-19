#include "binding/VectorCell.h"
#include "util/rangeAssertions.h"

extern "C"
{

using namespace lliby;

// This creates a vector cell without initialising its elements
VectorCell* llcore_vector_alloc(World &world, std::int64_t length)
{
	assertLengthValid(world, "(make-vector)", "vector length", VectorCell::maximumLength(), length);

	auto elements = new AnyCell*[length];

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) VectorCell(elements, length);
}

}
