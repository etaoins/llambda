#include "BytevectorCell.h"
#include "StringCell.h"

#include "alloc/allocator.h"

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

BytevectorCell* BytevectorCell::fromOwnedData(World &world, std::uint8_t *data, std::uint32_t length)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) BytevectorCell(data, length);
}
	
BytevectorCell* BytevectorCell::fromUnownedData(World &world, const std::uint8_t *data, std::uint32_t length)
{
	auto newData = new std::uint8_t[length];
	memcpy(newData, data, length);

	return BytevectorCell::fromOwnedData(world, newData, length);
}
	
BytevectorCell* BytevectorCell::fromFill(World &world, std::uint32_t length, std::uint8_t fill)
{
	auto newData = new std::uint8_t[length];
	memset(newData, fill, length);

	return BytevectorCell::fromOwnedData(world, newData, length);
}
	
BytevectorCell* BytevectorCell::fromAppended(World &world, const std::list<const BytevectorCell*> &byteVectors)
{
	std::uint64_t totalLength = 0;

	for(auto byteVector : byteVectors)
	{
		totalLength += byteVector->length();
	}

	if (totalLength > std::numeric_limits<std::uint32_t>::max())
	{
		return nullptr;
	}

	auto newData = new std::uint8_t[totalLength];
	std::uint8_t *copyPtr = newData;

	for(auto byteVector : byteVectors)
	{
		memcpy(copyPtr, byteVector->data(), byteVector->length());
		copyPtr += byteVector->length();
	}

	return BytevectorCell::fromOwnedData(world, newData, totalLength);
}
	
BytevectorCell* BytevectorCell::copy(World &world, std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}

	std::uint32_t newLength = end - start;
	auto newData = new std::uint8_t[newLength];

	memcpy(newData, &data()[start], newLength);

	return BytevectorCell::fromOwnedData(world, newData, newLength);
}
	
bool BytevectorCell::replace(std::uint32_t offset, const BytevectorCell *from, std::int64_t fromStart, std::int64_t fromEnd)
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

	memmove(&data()[offset], &from->data()[fromStart], replacedLength);

	return true;
}
	
StringCell* BytevectorCell::utf8ToString(World &world, std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}

	return StringCell::fromUtf8Data(world, &data()[start], end - start);
}

void BytevectorCell::finalizeBytevector()
{
	delete[] m_data;
}

}
