#ifndef _LLIBY_BINDING_BOXEDPROCEDURE_H
#define _LLIBY_BINDING_BOXEDPROCEDURE_H

#include "BoxedRecordLike.h"

namespace lliby
{

class BoxedProcedure : public BoxedRecordLike
{
#include "generated/BoxedProcedureMembers.h"
public:
	BoxedProcedure(std::uint32_t recordClassId, void *recordData, ProcedureEntryPoint entryPoint) :
		BoxedRecordLike(BoxedTypeId::Procedure, recordClassId, recordData),
		m_entryPoint(entryPoint)
	{
	}

	BoxedDatum* invoke(BoxedListElement *arguments);
};

}

#endif

