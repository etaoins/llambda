#include "VectorCell.h"
#include "UnitCell.h"

#include <limits>
#include <string.h>

#include "alloc/cellref.h"
#include "alloc/allocator.h"

#include "util/adjustSlice.h"

namespace lliby
{

bool VectorCell::fill(AnyCell *fill, SliceIndexType start, SliceIndexType end)
{
	// Fill doesn't need to be rooted because we have no allocations
	if (!adjustSlice(start, end, length()))
	{
		return false;
	}

	assert(!isGlobalConstant());
	for(LengthType i = start; i < end; i++)
	{
		elements()[i] = fill;
	}

	return true;
}

VectorCell* VectorCell::fromElements(World &world, AnyCell **elements, LengthType length)
{
	// Make sure our elements array is GC rooted for the next allocation
	alloc::StrongRoot<AnyCell> newElementsRoot(world, elements, length);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) VectorCell(elements, length);
}

VectorCell* VectorCell::fromFill(World &world, LengthType length, AnyCell *fill)
{
	alloc::AnyRef fillRef(world, fill);
	AnyCell **newElements;

	try
	{
		newElements = new AnyCell*[length];
	}
	catch(std::bad_alloc &)
	{
		return nullptr;
	}

	void *cellPlacement = alloc::allocateCells(world);
	auto newVector = new (cellPlacement) VectorCell(newElements, length);

	if (fillRef.isNull())
	{
		fillRef = const_cast<UnitCell*>(UnitCell::instance());
	}

	newVector->fill(fillRef, 0, -1);

	return newVector;
}

VectorCell* VectorCell::fromAppended(World &world, const std::vector<const VectorCell*> &vectors)
{
	std::int64_t totalLength = 0;

	for(auto vector : vectors)
	{
		totalLength += vector->length();
	}

	if (totalLength > maximumLength())
	{
		return nullptr;
	}

	auto newElements = new AnyCell*[totalLength];
	AnyCell **copyPtr = newElements;

	for(auto vector : vectors)
	{
		auto bytesToCopy = static_cast<std::size_t>(vector->length()) * sizeof(AnyCell*);

		memcpy(copyPtr, vector->elements(), bytesToCopy);
		copyPtr += vector->length();
	}

	// Root our elements in case allocating the vector cell triggers GC
	alloc::StrongRoot<AnyCell> newElementsRoot(world, newElements, totalLength);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) VectorCell(newElements, totalLength);
}

VectorCell* VectorCell::copy(World &world, SliceIndexType start, SliceIndexType end)
{
	if (!adjustSlice(start, end, length()))
	{
		return nullptr;
	}

	LengthType newLength = end - start;
	auto newElements = new AnyCell*[newLength];

	memcpy(newElements, &elements()[start], newLength * sizeof(AnyCell*));

	alloc::StrongRoot<AnyCell> newElementsRoot(world, newElements, newLength);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) VectorCell(newElements, newLength);
}

bool VectorCell::replace(SliceIndexType offset, const VectorCell *from, SliceIndexType fromStart, SliceIndexType fromEnd)
{
	if (!adjustSlice(fromStart, fromEnd, from->length()))
	{
		return false;
	}

	const LengthType replacedLength = fromEnd - fromStart;

	if ((replacedLength + offset) > length())
	{
		return false;
	}

	assert(!isGlobalConstant());
	memmove(&elements()[offset], &from->elements()[fromStart], replacedLength * sizeof(AnyCell*));

	return true;
}

void VectorCell::finalizeVector()
{
	delete[] m_elements;
}

}
