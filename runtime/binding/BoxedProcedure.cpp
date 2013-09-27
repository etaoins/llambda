#include "BoxedProcedure.h"

namespace lliby
{

BoxedDatum* BoxedProcedure::invoke(BoxedDatum *arguments)
{
	return m_entryPoint(m_closure, arguments);
}

}
