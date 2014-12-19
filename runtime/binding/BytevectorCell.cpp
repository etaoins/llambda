#include "BytevectorCell.h"
#include "StringCell.h"

#include "alloc/allocator.h"

#include "util/adjustSlice.h"

#include <limits>
#include <string.h>

namespace lliby
{

BytevectorCell* BytevectorCell::withByteArray(World &world, SharedByteArray *byteArray, LengthType length)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) BytevectorCell(byteArray, length);
}

BytevectorCell* BytevectorCell::fromData(World &world, const std::uint8_t *data, LengthType length)
{
	SharedByteArray *newByteArray = SharedByteArray::createInstance(length);
	memcpy(newByteArray->data(), data, length);

	return BytevectorCell::withByteArray(world, newByteArray, length);
}

BytevectorCell* BytevectorCell::fromFill(World &world, LengthType length, std::uint8_t fill)
{
	SharedByteArray *newByteArray = SharedByteArray::createInstance(length);

	if (newByteArray == nullptr)
	{
		return nullptr;
	}

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

	if (totalLength > maximumLength())
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

BytevectorCell* BytevectorCell::copy(World &world, SliceIndexType start, SliceIndexType end) const
{
	if (!adjustSlice(start, end, length()))
	{
		return nullptr;
	}

	if ((start == 0) && (end == length()))
	{
		// We can do a copy-on-write here
		return BytevectorCell::withByteArray(world, byteArray()->ref(), length());
	}

	const LengthType newLength = end - start;
	SharedByteArray *newByteArray = SharedByteArray::createInstance(newLength);

	memcpy(newByteArray->data(), &byteArray()->data()[start], newLength);

	return BytevectorCell::withByteArray(world, newByteArray, newLength);
}

bool BytevectorCell::replace(LengthType offset, const BytevectorCell *from, SliceIndexType fromStart, SliceIndexType fromEnd)
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

	// Break any COW
	m_byteArray = m_byteArray->asWritable(length());
	
	memmove(&byteArray()->data()[offset], &from->byteArray()->data()[fromStart], replacedLength);

	return true;
}

StringCell* BytevectorCell::utf8ToString(World &world, SliceIndexType start, SliceIndexType end)
{
	if (!adjustSlice(start, end, length()))
	{
		return nullptr;
	}
	
	if ((start == 0) && (end == length()))
	{
		// We can share our byte array
		return StringCell::withUtf8ByteArray(world, byteArray(), length());
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
