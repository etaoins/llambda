#include "ProcedureValue.h"

namespace lliby
{

BoxedDatum* ProcedureValue::invoke(BoxedDatum *arguments)
{
	return m_entryPoint(m_closure, arguments);
}

}
