#ifndef _LLIBY_BINDING_RECORDLIKECELL_H
#define _LLIBY_BINDING_RECORDLIKECELL_H

#include "AnyCell.h"

#include <vector>

extern "C"
{
	struct RecordClassMap;
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
	using RecordClassIdType = decltype(m_recordClassId);

	static void *allocateRecordData(size_t bytes);
	static void freeRecordData(void *);

	// Used by the garbage collector to update any references to record data stored inline
	void** recordDataRef()
	{
		return &m_recordData;
	}

	const RecordClassMap* classMap() const;
	RecordLikeDataStorage dataStorage() const;

	void finalizeRecordLike();

	/**
	 * Registers a runtime-created record-like class
	 *
	 * @param  totalSize  Total size of the of the record-like
	 * @param  offsets    List of offsets of AnyCells inside the record-like data
	 * @return Unique class ID for the new record-like class
	 */
	static RecordClassIdType registerRuntimeRecordClass(size_t totalSize, const std::vector<size_t> &offsets);

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
	RecordLikeCell(CellTypeId typeId, RecordClassIdType recordClassId, bool dataIsInline, void *recordData) :
		AnyCell(typeId),
		m_dataIsInline(dataIsInline),
		m_isUndefined(false),
		m_recordClassId(recordClassId),
		m_recordData(recordData)
	{
	}

	RecordLikeCell(CellTypeId typeId, RecordClassIdType recordClassId, bool dataIsInline, void *recordData, GarbageState gcState) :
		AnyCell(typeId, gcState),
		m_dataIsInline(dataIsInline),
		m_isUndefined(false),
		m_recordClassId(recordClassId),
		m_recordData(recordData)
	{
	}

	// TypeGenerator.scala always allocates this first
	static const RecordClassIdType EmptyRecordLikeClassId = 0;
};

}

#endif
