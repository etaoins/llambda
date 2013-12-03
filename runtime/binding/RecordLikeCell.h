#ifndef _LLIBY_BINDING_RECORDLIKECELL_H
#define _LLIBY_BINDING_RECORDLIKECELL_H

#include "DatumCell.h"

namespace lliby
{

class RecordLikeCell : public DatumCell
{
#include "generated/RecordLikeCellMembers.h"
public:
	void finalize();

protected:
	RecordLikeCell(CellTypeId typeId, std::uint32_t recordClassId, void *recordData) :
		DatumCell(typeId),
		m_recordClassId(recordClassId),
		m_recordData(recordData)
	{
	}
};

}

#endif
