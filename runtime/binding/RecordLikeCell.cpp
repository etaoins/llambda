#include "RecordLikeCell.h"

#include <stdlib.h>

namespace lliby
{
	
void RecordLikeCell::finalize()
{
	free(m_recordData);
}

}
