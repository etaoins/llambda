#include "binding/DatumCell.h"
#include "binding/VectorCell.h"
#include "binding/ProperList.h"

#include "alloc/StrongRef.h"

#include "core/error.h"

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

DatumCell* lliby_vector_ref(VectorCell *vector, std::uint32_t index)
{
	DatumCell* element = vector->elementAt(index);

	if (element == nullptr)
	{
		signalError("Vector index out of bounds", {vector});	
	}

	return element;
}

void lliby_vector_set(VectorCell *vector, std::uint32_t index, DatumCell *obj)
{
	if (!vector->setElementAt(index, obj))
	{
		signalError("Vector index out of bounds", {vector});	
	}
}

VectorCell *lliby_vector(ListElementCell *argHead)
{
	ProperList<DatumCell> properList(argHead);
	
	if (!properList.isValid())
	{
		signalError("Non-list passed to (list->vector)", {argHead}); 
	}

	auto length = properList.length();
	auto newElements = new DatumCell*[length];
	unsigned int elementIndex = 0;

	// Fill out the new elements from the list
	for(auto element : properList)
	{
		newElements[elementIndex++] = element;
	}

	// Make sure our elements array is GC rooted for the next allocation
	alloc::StrongRefRange<DatumCell> newElementsRoot(newElements, length);

	// Return the new vector
	return new VectorCell(newElements, length);
}

VectorCell *lliby_vector_append(ListElementCell *argHead)
{
	ProperList<VectorCell> argList(argHead);
	
	if (!argList.isValid())
	{
		signalError("Non-vector passed to (vector-append)", {argHead}); 
	}

	// Create a std::vector
	auto vectorElements = std::vector<const VectorCell*>(argList.begin(), argList.end());

	// Append the vectors
	return VectorCell::fromAppended(vectorElements);
}

}
