#include "ByteVectorValue.h"
#include "StringValue.h"

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
	
ByteVectorValue* ByteVectorValue::fromFill(std::uint32_t length, std::uint8_t fill)
{
	auto newData = new std::uint8_t[length];
	memset(newData, fill, length);

	return new ByteVectorValue(newData, length);
}
	
ByteVectorValue* ByteVectorValue::fromAppended(const std::list<const ByteVectorValue*> &byteVectors)
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

	return new ByteVectorValue(newData, totalLength);
}
	
ByteVectorValue* ByteVectorValue::copy(std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}

	std::uint32_t newLength = end - start;
	auto newData = new std::uint8_t[newLength];

	memcpy(newData, &data()[start], newLength);

	return new ByteVectorValue(newData, newLength);
}
	
bool ByteVectorValue::replace(std::uint32_t offset, const ByteVectorValue *from, std::int64_t fromStart, std::int64_t fromEnd)
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
	
StringValue* ByteVectorValue::utf8ToString(std::int64_t start, std::int64_t end)
{
	if (!adjustRange(start, end, length()))
	{
		return nullptr;
	}

	return StringValue::fromUtf8Data(&data()[start], end - start);
}

void ByteVectorValue::finalize()
{
	delete[] m_data;
}

}
