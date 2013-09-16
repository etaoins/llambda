#include "VectorValue.h"
#include "UnspecificValue.h"

#include <limits>

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

bool VectorValue::fill(BoxedDatum *fill, std::int64_t start, std::int64_t end)
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
	
VectorValue* VectorValue::fromFill(std::uint32_t length, BoxedDatum *fill)
{
	auto newElements = new BoxedDatum*[length];

	auto newVector = new VectorValue(newElements, length);

	if (fill == nullptr)
	{
		fill = const_cast<UnspecificValue*>(UnspecificValue::instance());
	}

	newVector->fill(fill, 0, -1);

	return newVector;
}

VectorValue* VectorValue::fromAppended(const std::list<const VectorValue*> &vectors)
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

	return new VectorValue(newElements, totalLength);
}

VectorValue* VectorValue::copy(std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}

	std::uint32_t newLength = end - start;
	auto newElements = new BoxedDatum*[newLength];

	memcpy(newElements, &elements()[start], newLength * sizeof(BoxedDatum*));

	return new VectorValue(newElements, newLength);
}

bool VectorValue::replace(std::uint32_t offset, const VectorValue *from, std::int64_t fromStart, std::int64_t fromEnd)
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
