#include "RecordLikeCell.h"

#include <stdlib.h>
#include <vector>

#include "classmap/RecordClassMap.h"

namespace lliby
{

namespace
{

const std::uint32_t RuntimeRecordClassFlag = 1 << 31; 
std::vector<RecordClassOffsetMap *> runtimeRecordClassOffsets;

}

void* RecordLikeCell::allocateRecordData(size_t bytes)
{
	return malloc(bytes);
}
	
void RecordLikeCell::finalizeRecordLike()
{
	if (!dataIsInline())
	{
		free(m_recordData);
	}
}
	
const RecordClassOffsetMap* RecordLikeCell::offsetMap() const
{
	if (recordClassId() & RuntimeRecordClassFlag)
	{
		const std::uint32_t rawClassId = recordClassId() & ~RuntimeRecordClassFlag;
		return runtimeRecordClassOffsets[rawClassId];
	}
	else
	{
		return _llambda_compiler_class_map[recordClassId()];
	}
}
	
std::uint32_t RecordLikeCell::registerRuntimeRecordClass(const std::vector<size_t> &offsets)
{
	// The raw class ID is the index in the runtimeRecordClassOffsets vector
	const std::uint32_t offsetCount = offsets.size();
	const std::uint32_t rawClassId = runtimeRecordClassOffsets.size();

	// Create the offset map
	RecordClassOffsetMap *offsetMap = nullptr;
	
	if (offsetCount > 0)
	{
		offsetMap = static_cast<RecordClassOffsetMap*>(malloc(sizeof(std::uint32_t) * (offsetCount + 1)));

		offsetMap->offsetCount = offsetCount;

		for(std::uint32_t i = 0; i < offsetCount; i++)
		{
			offsetMap->offsets[i] = offsets[i];
		}
	}

	runtimeRecordClassOffsets.push_back(offsetMap);

	return rawClassId | RuntimeRecordClassFlag;
}

}
