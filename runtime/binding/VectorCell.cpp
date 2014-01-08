#include "VectorCell.h"
#include "UnitCell.h"

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

bool VectorCell::fill(DatumCell *fill, std::int64_t start, std::int64_t end)
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
	
VectorCell* VectorCell::fromFill(std::uint32_t length, DatumCell *fill)
{
	auto newElements = new DatumCell*[length];

	auto newVector = new VectorCell(newElements, length);

	if (fill == nullptr)
	{
		fill = const_cast<UnitCell*>(UnitCell::instance());
	}

	newVector->fill(fill, 0, -1);

	return newVector;
}

VectorCell* VectorCell::fromAppended(const std::list<const VectorCell*> &vectors)
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

	auto newElements = new DatumCell*[totalLength];
	DatumCell **copyPtr = newElements;

	for(auto vector : vectors)
	{
		auto bytesToCopy = static_cast<size_t>(vector->length()) * sizeof(DatumCell*);

		memcpy(copyPtr, vector->elements(), bytesToCopy);
		copyPtr += vector->length();
	}

	return new VectorCell(newElements, totalLength);
}

VectorCell* VectorCell::copy(std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}

	std::uint32_t newLength = end - start;
	auto newElements = new DatumCell*[newLength];

	memcpy(newElements, &elements()[start], newLength * sizeof(DatumCell*));

	return new VectorCell(newElements, newLength);
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

	memmove(&elements()[offset], &from->elements()[fromStart], replacedLength * sizeof(DatumCell*));

	return true;
}

void VectorCell::finalize()
{
	delete[] m_elements;
}

}
