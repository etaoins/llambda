#include "BoxedVectorLike.h"

namespace lliby
{

void BoxedVectorLike::finalize()
{
	delete[] m_elements;
}

}
