#ifndef _LLIBY_BINDING_BOXEDPROCEDURE_H
#define _LLIBY_BINDING_BOXEDPROCEDURE_H

#include "BoxedDatum.h"

namespace lliby
{

class BoxedProcedure : public BoxedDatum
{
#include "generated/BoxedProcedureMembers.h"
public:
	BoxedProcedure(BoxedClosure *closure, ProcedureEntryPoint entryPoint) :
		BoxedDatum(BoxedTypeId::Procedure),
		m_closure(closure),
		m_entryPoint(entryPoint)
	{
	}

	BoxedDatum* invoke(BoxedDatum *arguments);
};

}

#endif

