#include "binding/AnyCell.h"

extern "C"
{

using namespace lliby;

AnyCell** _lliby_vector_elements_alloc(std::uint32_t length)
{
	return new AnyCell*[length];
}

}
