#include <stdlib.h>
#include <cstdint>

#include "binding/RecordLikeCell.h"

extern "C"
{

using namespace lliby;

void *llcore_record_data_alloc(std::uint64_t size)
{
	return RecordLikeCell::allocateRecordData(size);
}

}
