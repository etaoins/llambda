#include "RecordLikeCell.h"

#include <stdlib.h>

#include "classmap/RecordClassMap.h"

namespace lliby
{
	
void RecordLikeCell::finalizeRecordLike()
{
	if (!dataIsInline())
	{
		free(m_recordData);
	}
}
	
const RecordClassOffsetMap* RecordLikeCell::offsetMap() const
{
	return _llambda_class_map[recordClassId()];
}

}
