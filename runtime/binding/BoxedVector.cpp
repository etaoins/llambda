#include "BoxedVector.h"
#include "BoxedUnspecific.h"

#include <limits>
#include <string.h>

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

bool BoxedVector::fill(BoxedDatum *fill, std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return false;
	}
	
	for(std::uint32_t i = start; i < end; i++)
	{
		elements()[i] = fill;
	}

	return true;
}
	
BoxedVector* BoxedVector::fromFill(std::uint32_t length, BoxedDatum *fill)
{
	auto newElements = new BoxedDatum*[length];

	auto newVector = new BoxedVector(newElements, length);

	if (fill == nullptr)
	{
		fill = const_cast<BoxedUnspecific*>(BoxedUnspecific::instance());
	}

	newVector->fill(fill, 0, -1);

	return newVector;
}

BoxedVector* BoxedVector::fromAppended(const std::list<const BoxedVector*> &vectors)
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

	auto newElements = new BoxedDatum*[totalLength];
	BoxedDatum **copyPtr = newElements;

	for(auto vector : vectors)
	{
		auto bytesToCopy = static_cast<size_t>(vector->length()) * sizeof(BoxedDatum*);

		memcpy(copyPtr, vector->elements(), bytesToCopy);
		copyPtr += vector->length();
	}

	return new BoxedVector(newElements, totalLength);
}

BoxedVector* BoxedVector::copy(std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}

	std::uint32_t newLength = end - start;
	auto newElements = new BoxedDatum*[newLength];

	memcpy(newElements, &elements()[start], newLength * sizeof(BoxedDatum*));

	return new BoxedVector(newElements, newLength);
}

bool BoxedVector::replace(std::uint32_t offset, const BoxedVector *from, std::int64_t fromStart, std::int64_t fromEnd)
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

	memmove(&elements()[offset], &from->elements()[fromStart], replacedLength * sizeof(BoxedDatum*));

	return true;
}

}
