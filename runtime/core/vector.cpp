#include "binding/AnyCell.h"

extern "C"
{

using namespace lliby;

AnyCell** llcore_vector_elements_alloc(std::uint32_t length)
{
	return new AnyCell*[length];
}

}
