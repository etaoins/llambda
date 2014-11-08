#include "VectorCell.h"
#include "UnitCell.h"

#include <limits>
#include <string.h>

#include "alloc/cellref.h"
#include "alloc/allocator.h"

namespace
{
	bool adjustRange(std::int64_t start, std::int64_t &end, std::uint32_t length)
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

		return true;
	}
}

namespace lliby
{

bool VectorCell::fill(AnyCell *fill, std::int64_t start, std::int64_t end)
{
	// Fill doesn't need to be rooted because we have no allocations
	if (!adjustRange(start, end, length()))
	{
		return false;
	}
	
	assert(!isGlobalConstant());
	for(std::uint32_t i = start; i < end; i++)
	{
		elements()[i] = fill;
	}

	return true;
}

VectorCell* VectorCell::fromElements(World &world, AnyCell **elements, std::uint32_t length)
{
	// Make sure our elements array is GC rooted for the next allocation
	alloc::StrongRoot<AnyCell> newElementsRoot(world, elements, length);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) VectorCell(elements, length);
}
	
VectorCell* VectorCell::fromFill(World &world, std::uint32_t length, AnyCell *fill)
{
	alloc::AnyRef fillRef(world, fill);
	auto newElements = new AnyCell*[length];

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
	std::uint64_t totalLength = 0;

	for(auto vector : vectors)
	{
		totalLength += vector->length();
	}

	if (totalLength > std::numeric_limits<std::uint32_t>::max())
	{
		return nullptr;
	}

	auto newElements = new AnyCell*[totalLength];
	AnyCell **copyPtr = newElements;

	for(auto vector : vectors)
	{
		auto bytesToCopy = static_cast<size_t>(vector->length()) * sizeof(AnyCell*);

		memcpy(copyPtr, vector->elements(), bytesToCopy);
		copyPtr += vector->length();
	}

	// Root our elements in case allocating the vector cell triggers GC
	alloc::StrongRoot<AnyCell> newElementsRoot(world, newElements, totalLength);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) VectorCell(newElements, totalLength);
}

VectorCell* VectorCell::copy(World &world, std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}

	std::uint32_t newLength = end - start;
	auto newElements = new AnyCell*[newLength];

	memcpy(newElements, &elements()[start], newLength * sizeof(AnyCell*));

	alloc::StrongRoot<AnyCell> newElementsRoot(world, newElements, newLength);

	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) VectorCell(newElements, newLength);
}

bool VectorCell::replace(std::uint32_t offset, const VectorCell *from, std::int64_t fromStart, std::int64_t fromEnd)
{
	if (!adjustRange(fromStart, fromEnd, from->length()))
	{
		return false;
	}

	const std::uint32_t replacedLength = fromEnd - fromStart;

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
