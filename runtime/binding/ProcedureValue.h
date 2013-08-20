#ifndef _LLIBY_BINDING_PROCEDUREVALUE_H
#define _LLIBY_BINDING_PROCEDUREVALUE_H

#include "BoxedDatum.h"

namespace lliby
{

class ProcedureValue : public BoxedDatum
{
#include "generated/ProcedureValueMembers.h"
public:
	ProcedureValue(ClosureValue *closure, ProcedureEntryPoint entryPoint) :
		BoxedDatum(BoxedTypeId::Procedure),
		m_closure(closure),
		m_entryPoint(entryPoint)
	{
	}

	BoxedDatum* invoke(BoxedDatum *arguments);
};

}

#endif

