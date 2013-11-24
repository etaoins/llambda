#ifndef _LLIBY_BINDING_BOXEDRECORD_H
#define _LLIBY_BINDING_BOXEDRECORD_H

#include "BoxedRecordLike.h"

namespace lliby
{

class BoxedRecord : public BoxedRecordLike
{
#include "generated/BoxedRecordMembers.h"
public:
	BoxedRecord(std::uint32_t recordClassId, void *recordData) :
		BoxedRecordLike(BoxedTypeId::Record, recordClassId, recordData)
	{
	}
};

}

#endif

