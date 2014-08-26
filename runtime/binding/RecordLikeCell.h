#ifndef _LLIBY_BINDING_RECORDLIKECELL_H
#define _LLIBY_BINDING_RECORDLIKECELL_H

#include "AnyCell.h"

#include <vector>

extern "C"
{
	struct RecordClassOffsetMap;
}

namespace lliby
{

enum class RecordLikeDataStorage
{
	Empty,
	Inline,
	OutOfLne
};

class RecordLikeCell : public AnyCell
{
#include "generated/RecordLikeCellMembers.h"
public:
	static void *allocateRecordData(size_t bytes);
	void finalize();

	// Used by the garbage collector to update any references to record data stored inline
	void** recordDataRef()
	{
		return &m_recordData;
	}

	const RecordClassOffsetMap* offsetMap() const;
	RecordLikeDataStorage dataStorage() const;
	
	void finalizeRecordLike();
	
	/**
	 * Registers a runtime-created record-like class
	 *
	 * @param  offsets  List of offsets of AnyCells inside the record-like data
	 * @return Unique class ID for the new record-like class 
	 */
	static std::uint32_t registerRuntimeRecordClass(const std::vector<size_t> &offsets);

	void setRecordData(void *newData)
	{
		m_recordData = newData;
	}

	/**
	 * Returns the number of active record data allocations
	 *
	 * If leak checking is disabled this always returns 0
	 *
	 * This is not synchronized with other threads. For that reason this value is only accurate when there is no
	 * concurrent instance creation or destruction and any other previously modifying threads have been synchronized
	 * with through another mechanism.
	 */
	static size_t recordDataInstanceCount();

protected:
	RecordLikeCell(CellTypeId typeId, std::uint32_t recordClassId, bool dataIsInline, void *recordData) :
		AnyCell(typeId),
		m_dataIsInline(dataIsInline),
		m_isUndefined(false),
		m_recordClassId(recordClassId),
		m_recordData(recordData)
	{
	}
	
	// TypeGenerator.scala always allocates this first
	static const std::uint32_t EmptyClosureRecordClassId = 0;
};

}

#endif
