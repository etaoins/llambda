#ifndef _LLIBY_BINDING_RECORDLIKECELL_H
#define _LLIBY_BINDING_RECORDLIKECELL_H

#include "DatumCell.h"

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

class RecordLikeCell : public DatumCell
{
#include "generated/RecordLikeCellMembers.h"
public:
	void finalize();

	// Used by the garbage collector to update any references to record data stored inline
	void** recordDataRef()
	{
		return &m_recordData;
	}

	const RecordClassOffsetMap* offsetMap() const;
	RecordLikeDataStorage dataStorage() const;
	
	void finalizeRecordLike();

protected:
	RecordLikeCell(CellTypeId typeId, std::uint32_t recordClassId, void *recordData) :
		DatumCell(typeId),
		m_recordClassId(recordClassId),
		m_recordData(recordData)
	{
	}
	
	// TypeGenerator.scala always allocates this first
	static const std::uint32_t EmptyClosureRecordClassId = 0;
};

}

#endif
