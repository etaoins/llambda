#include "binding/DatumCell.h"
#include "binding/VectorCell.h"
#include "binding/ProperList.h"

#include "core/fatal.h"

using namespace lliby;

extern "C"
{

VectorCell *lliby_make_vector(std::uint32_t length, DatumCell *fill)
{
	return VectorCell::fromFill(length, fill);
}

std::uint32_t lliby_vector_length(VectorCell *vector)
{
	return vector->length();
}

DatumCell* lliby_vector_ref(const VectorCell *vector, std::uint32_t index)
{
	DatumCell* element = vector->elementAt(index);

	if (element == nullptr)
	{
		_lliby_fatal("Vector index out of bounds", vector);	
	}

	return element;
}

void lliby_vector_set(VectorCell *vector, std::uint32_t index, DatumCell *obj)
{
	if (!vector->setElementAt(index, obj))
	{
		_lliby_fatal("Vector index out of bounds", vector);	
	}
}

VectorCell *lliby_vector(ListElementCell *argHead)
{
	ProperList<DatumCell> properList(argHead);
	
	if (!properList.isValid())
	{
		_lliby_fatal("Non-list passed to (list->vector)", argHead); 
	}

	auto length = properList.length();
	auto newElements = new DatumCell*[length];
	unsigned int elementIndex = 0;

	// Fill out the new elements from the list
	for(auto element : properList)
	{
		newElements[elementIndex++] = element;
	}

	// Return the new vector
	return new VectorCell(newElements, length);
}

VectorCell *lliby_vector_append(ListElementCell *argHead)
{
	ProperList<VectorCell> argList(argHead);
	
	if (!argList.isValid())
	{
		_lliby_fatal("Non-vector passed to (vector-append)", argHead); 
	}

	// Create a std::list
	auto vectorList = std::list<const VectorCell*>(argList.begin(), argList.end());

	// Append the vectors
	return VectorCell::fromAppended(vectorList);
}

}
