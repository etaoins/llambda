#include "ProcedureValue.h"

namespace lliby
{

BoxedValue* ProcedureValue::invoke(BoxedValue *arguments)
{
	return m_entryPoint(m_closure, arguments);
}

}
