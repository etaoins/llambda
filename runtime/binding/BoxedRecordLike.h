#ifndef _LLIBY_BINDING_BOXEDRECORDLIKE_H
#define _LLIBY_BINDING_BOXEDRECORDLIKE_H

#include "BoxedDatum.h"

namespace lliby
{

class BoxedRecordLike : public BoxedDatum
{
#include "generated/BoxedRecordLikeMembers.h"
public:
	void finalize();

protected:
	BoxedRecordLike(BoxedTypeId typeId, std::uint32_t recordClassId, void *recordData) :
		BoxedDatum(typeId),
		m_recordClassId(recordClassId),
		m_recordData(recordData)
	{
	}
};

}

#endif
