#include "binding/BoxedDatum.h"
#include "binding/BoxedVector.h"
#include "binding/ProperList.h"

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

BoxedVector *lliby_vector(BoxedListElement *argHead)
{
	ProperList<BoxedDatum> properList(argHead);
	
	if (!properList.isValid())
	{
		_lliby_fatal("Non-list passed to (list->vector)", argHead); 
	}

	auto length = properList.length();
	auto newElements = new BoxedDatum*[length];
	unsigned int elementIndex = 0;

	// Fill out the new elements from the list
	for(auto element : properList)
	{
		newElements[elementIndex++] = element;
	}

	// Return the new vector
	return new BoxedVector(newElements, length);
}

BoxedVector *lliby_vector_append(BoxedListElement *argHead)
{
	ProperList<BoxedVector> argList(argHead);
	
	if (!argList.isValid())
	{
		_lliby_fatal("Non-vector passed to (vector-append)", argHead); 
	}

	// Create a std::list
	auto vectorList = std::list<const BoxedVector*>(argList.begin(), argList.end());

	// Append the vectors
	return BoxedVector::fromAppended(vectorList);
}

}
