#ifndef _LLIBY_BINDING_PROCEDUREVALUE_H
#define _LLIBY_BINDING_PROCEDUREVALUE_H

#include "BoxedValue.h"

namespace lliby
{

class ProcedureValue : public BoxedValue
{
#include "generated/ProcedureValueMembers.h"
public:
	ProcedureValue(ClosureValue *closure, ProcedureEntryPoint entryPoint) :
		BoxedValue(BoxedTypeId::Procedure),
		m_closure(closure),
		m_entryPoint(entryPoint)
	{
	}

	BoxedValue* invoke(BoxedValue *arguments);
};

}

#endif

