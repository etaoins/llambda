#include "RecordLikeCell.h"

#include <stdlib.h>
#include <vector>
#include <atomic>

#include "classmap/RecordClassMap.h"
#include "dynamic/State.h"

namespace lliby
{

namespace
{
	const std::uint32_t RuntimeRecordClassFlag = 1 << 31;
	std::vector<RecordClassMap *> runtimeRecordClassMaps;

#ifdef _LLIBY_CHECK_LEAKS
	std::atomic<size_t> recordDataAllocCount(0);
#endif
}

void* RecordLikeCell::allocateRecordData(size_t bytes)
{
#ifdef _LLIBY_CHECK_LEAKS
	recordDataAllocCount.fetch_add(1, std::memory_order_relaxed);
#endif

	return malloc(bytes);
}

void RecordLikeCell::finalizeRecordLike()
{
	if (!dataIsInline())
	{
#ifdef _LLIBY_CHECK_LEAKS
		recordDataAllocCount.fetch_sub(1, std::memory_order_relaxed);
#endif
		free(m_recordData);
	}
}

size_t RecordLikeCell::recordDataInstanceCount()
{
#ifdef _LLIBY_CHECK_LEAKS
	return recordDataAllocCount.load(std::memory_order_relaxed);
#else
	return 0;
#endif
}

const RecordClassMap* RecordLikeCell::classMap() const
{
	if (recordClassId() & RuntimeRecordClassFlag)
	{
		const RecordClassIdType rawClassId = recordClassId() & ~RuntimeRecordClassFlag;
		return runtimeRecordClassMaps[rawClassId];
	}
	else
	{
		return _llambda_compiler_class_map[recordClassId()];
	}
}

RecordLikeCell::RecordClassIdType RecordLikeCell::registerRuntimeRecordClass(size_t totalSize, const std::vector<size_t> &offsets)
{
	// The raw class ID is the index in the runtimeRecordClass vector
	const std::uint32_t offsetCount = offsets.size();
	const RecordClassIdType rawClassId = runtimeRecordClassMaps.size();

	// Create the offset map
	RecordClassMap *classMap = static_cast<RecordClassMap*>(malloc(sizeof(std::uint32_t) * (offsetCount + 2)));

	classMap->totalSize = totalSize;
	classMap->offsetCount = offsetCount;

	for(std::uint32_t i = 0; i < offsetCount; i++)
	{
		classMap->offsets[i] = offsets[i];
	}

	runtimeRecordClassMaps.push_back(classMap);

	return rawClassId | RuntimeRecordClassFlag;
}

}
