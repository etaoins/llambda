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

BytevectorCell* BytevectorCell::withByteArray(World &world, SharedByteArray *byteArray, std::uint32_t length)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) BytevectorCell(byteArray, length);
}
	
BytevectorCell* BytevectorCell::fromData(World &world, const std::uint8_t *data, std::uint32_t length)
{
	SharedByteArray *newByteArray = SharedByteArray::createInstance(length);
	memcpy(newByteArray->data(), data, length);

	return BytevectorCell::withByteArray(world, newByteArray, length);
}
	
BytevectorCell* BytevectorCell::fromFill(World &world, std::uint32_t length, std::uint8_t fill)
{
	SharedByteArray *newByteArray = SharedByteArray::createInstance(length);
	memset(newByteArray->data(), fill, length);

	return BytevectorCell::withByteArray(world, newByteArray, length);
}
	
BytevectorCell* BytevectorCell::fromAppended(World &world, const std::list<const BytevectorCell*> &byteVectors)
{
	if (byteVectors.size() == 1)
	{
		// This allows implicit data sharing while the below always allocates
		return byteVectors.front()->copy(world);
	}
	
	std::uint64_t totalLength = 0;

	for(auto byteVector : byteVectors)
	{
		totalLength += byteVector->length();
	}

	if (totalLength > std::numeric_limits<std::uint32_t>::max())
	{
		return nullptr;
	}

	SharedByteArray *newByteArray = SharedByteArray::createInstance(totalLength);
	std::uint8_t *copyPtr = newByteArray->data();

	for(auto byteVector : byteVectors)
	{
		memcpy(copyPtr, byteVector->byteArray()->data(), byteVector->length());
		copyPtr += byteVector->length();
	}

	return BytevectorCell::withByteArray(world, newByteArray, totalLength);
}
	
BytevectorCell* BytevectorCell::copy(World &world, std::int64_t start, std::int64_t end) const
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}

	if ((start == 0) && (end == length()))
	{
		// We can do a copy-on-write here
		return BytevectorCell::withByteArray(world, byteArray()->ref(), length());
	}
	
	const std::uint32_t newLength = end - start;
	SharedByteArray *newByteArray = SharedByteArray::createInstance(newLength);

	memcpy(newByteArray->data(), &byteArray()->data()[start], newLength);

	return BytevectorCell::withByteArray(world, newByteArray, newLength);
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

	// Break any COW
	m_byteArray = m_byteArray->asWritable(length());
	
	memmove(&byteArray()->data()[offset], &from->byteArray()->data()[fromStart], replacedLength);

	return true;
}
	
StringCell* BytevectorCell::utf8ToString(World &world, std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}
	
	if ((start == 0) && (end == length()))
	{
		// We can share our byte array
		return StringCell::withUtf8ByteArray(world, byteArray()->ref(), length());
	}
	else
	{
		return StringCell::fromUtf8Data(world, &byteArray()->data()[start], end - start);
	}
}

void BytevectorCell::finalizeBytevector()
{
	m_byteArray->unref();
}

}
