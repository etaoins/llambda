#include "binding/AnyCell.h"
#include "binding/VectorCell.h"
#include "binding/ProperList.h"
#include "binding/RestArgument.h"

#include "core/error.h"
#include "core/World.h"

#include "util/assertSliceValid.h"

using namespace lliby;

extern "C"
{

VectorCell *lliby_make_vector(World &world, std::uint32_t length, AnyCell *fill)
{
	return VectorCell::fromFill(world, length, fill);
}

std::uint32_t lliby_vector_length(VectorCell *vector)
{
	return vector->length();
}

AnyCell* lliby_vector_ref(World &world, VectorCell *vector, std::uint32_t index)
{
	AnyCell* element = vector->elementAt(index);

	if (element == nullptr)
	{
		signalError(world, "Vector index out of bounds", {vector});	
	}

	return element;
}

void lliby_vector_set(World &world, VectorCell *vector, std::uint32_t index, AnyCell *obj)
{
	if (vector->isGlobalConstant())
	{
		signalError(world, "(vector-set!) on vector literal", {vector});	
	}
	
	if (!vector->setElementAt(index, obj))
	{
		signalError(world, "Vector index out of bounds", {vector});	
	}
}

// Note we can't use RestArgument here because invalid lists can be passed in via our (list->vector) alias
VectorCell *lliby_vector(World &world, ListElementCell *argHead)
{
	ProperList<AnyCell> properList(argHead);
	
	if (!properList.isValid())
	{
		signalError(world, "Non-list passed to (list->vector)", {argHead}); 
	}

	auto length = properList.length();
	auto newElements = new AnyCell*[length];
	unsigned int elementIndex = 0;

	// Fill out the new elements from the list
	for(auto element : properList)
	{
		newElements[elementIndex++] = element;
	}

	// Return the new vector
	return VectorCell::fromElements(world, newElements, length);
}

VectorCell *lliby_vector_append(World &world, RestArgument<VectorCell> *argHead)
{
	ProperList<VectorCell> argList(argHead);
	
	// Create a std::vector
	auto vectorElements = std::vector<const VectorCell*>(argList.begin(), argList.end());

	// Append the vectors
	return VectorCell::fromAppended(world, vectorElements);
}

ListElementCell *lliby_vector_to_list(World &world, VectorCell *vectorCell, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world,"(vector->list)", vectorCell, vectorCell->length(), start, end);

	AnyCell **startPointer = vectorCell->elements() + start;
	AnyCell **endPointer = vectorCell->elements() + end;

	std::vector<AnyCell*> vectorElements(startPointer, endPointer);
	return ListElementCell::createProperList(world, vectorElements);
}

VectorCell *lliby_vector_copy(World &world, VectorCell *sourceVector, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world, "(vector-copy)", sourceVector, sourceVector->length(), start, end);

	return sourceVector->copy(world, start, end);
}

}
