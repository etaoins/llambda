#include <stdlib.h>
#include <cstdint>

extern "C"
{

void *_lliby_record_data_alloc(std::uint64_t size)
{
	return malloc(size);
}

}
