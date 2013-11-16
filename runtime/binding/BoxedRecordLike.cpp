#include "BoxedRecordLike.h"

#include <stdlib.h>

namespace lliby
{
	
void BoxedRecordLike::finalize()
{
	free(m_recordData);
}

}
