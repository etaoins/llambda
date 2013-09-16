#include "VectorLikeValue.h"

namespace lliby
{

void VectorLikeValue::finalize()
{
	delete[] m_elements;
}

}
