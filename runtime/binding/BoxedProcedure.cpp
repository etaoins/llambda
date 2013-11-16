#include "BoxedProcedure.h"

namespace lliby
{

BoxedDatum* BoxedProcedure::invoke(BoxedListElement *arguments)
{
	return m_entryPoint(recordData(), arguments);
}

}
