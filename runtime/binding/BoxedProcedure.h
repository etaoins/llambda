#ifndef _LLIBY_BINDING_BOXEDPROCEDURE_H
#define _LLIBY_BINDING_BOXEDPROCEDURE_H

#include "BoxedDatum.h"

namespace lliby
{

class BoxedProcedure : public BoxedDatum
{
#include "generated/BoxedProcedureMembers.h"
public:
	BoxedProcedure(std::uint32_t capturedDataLength, BoxedDatum ***capturedData, ProcedureEntryPoint entryPoint) :
		BoxedDatum(BoxedTypeId::Procedure),
		m_capturedDataLength(capturedDataLength),
		m_capturedData(capturedData),
		m_entryPoint(entryPoint)
	{
	}

	BoxedDatum* invoke(BoxedListElement *arguments);
};

}

#endif

