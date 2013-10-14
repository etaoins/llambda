#include "binding/BoxedDatum.h"
#include "binding/BoxedVector.h"

#include "core/fatal.h"

using namespace lliby;

extern "C"
{

BoxedVector *lliby_make_vector(std::uint32_t length, BoxedDatum *fill)
{
	return BoxedVector::fromFill(length, fill);
}

std::uint32_t lliby_vector_length(BoxedVector *vector)
{
	return vector->length();
}

BoxedDatum* lliby_vector_ref(const BoxedVector *vector, std::uint32_t index)
{
	BoxedDatum* element = vector->elementAt(index);

	if (element == nullptr)
	{
		_lliby_fatal("Vector index out of bounds", vector);	
	}

	return element;
}

void lliby_vector_set(BoxedVector *vector, std::uint32_t index, BoxedDatum *obj)
{
	if (!vector->setElementAt(index, obj))
	{
		_lliby_fatal("Vector index out of bounds", vector);	
	}
}

}
