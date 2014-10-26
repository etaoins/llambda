#include "binding/AnyCell.h"
#include "binding/VectorCell.h"
#include "binding/ProperList.h"
#include "binding/StringCell.h"
#include "binding/CharCell.h"

#include "core/error.h"
#include "core/World.h"

#include "unicode/UnicodeChar.h"

#include "util/assertSliceValid.h"
#include "util/StringCellBuilder.h"

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

VectorCell *lliby_vector(World &world, ProperList<AnyCell> *argList)
{
	const auto length = argList->size();

	auto newElements = new AnyCell*[length];
	std::copy(argList->begin(), argList->end(), newElements);

	// Return the new vector
	return VectorCell::fromElements(world, newElements, length);
}

VectorCell *lliby_vector_append(World &world, ProperList<VectorCell> *argList)
{
	// Create a std::vector
	auto vectorElements = std::vector<const VectorCell*>(argList->begin(), argList->end());

	// Append the vectors
	return VectorCell::fromAppended(world, vectorElements);
}

ProperList<AnyCell> *lliby_vector_to_list(World &world, VectorCell *vectorCell, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world,"(vector->list)", vectorCell, vectorCell->length(), start, end);

	AnyCell **startPointer = vectorCell->elements() + start;
	AnyCell **endPointer = vectorCell->elements() + end;

	std::vector<AnyCell*> vectorElements(startPointer, endPointer);
	return ProperList<AnyCell>::create(world, vectorElements);
}

VectorCell *lliby_vector_copy(World &world, VectorCell *sourceVector, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world, "(vector-copy)", sourceVector, sourceVector->length(), start, end);

	return sourceVector->copy(world, start, end);
}

void lliby_vector_mutating_fill(World &world, VectorCell *vector, AnyCell *fill, std::uint32_t start, std::uint32_t end)
{
	if (vector->isGlobalConstant())
	{
		signalError(world, "(vector-fill!) on vector literal", {vector});
	}

	assertSliceValid(world, "(vector-fill!)", vector, vector->length(), start, end);

	vector->fill(fill, start, end);
}

VectorCell* lliby_string_to_vector(World &world, StringCell *string, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world, "(string->vector)", string, string->charLength(), start, end);

	std::vector<UnicodeChar> unboxedChars(string->unicodeChars(start, end));
	const size_t charCount = unboxedChars.size();

	alloc::RangeAlloc allocation = alloc::allocateRange(world, charCount);
	auto allocIt = allocation.begin();

	AnyCell **boxedChars = new AnyCell*[charCount];
	auto unboxedIt = unboxedChars.begin();

	for(size_t i = 0; i < charCount; i++)
	{
		boxedChars[i] = new (*allocIt++) CharCell(*unboxedIt++);
	}

	return VectorCell::fromElements(world, boxedChars, charCount);
}

StringCell *lliby_vector_to_string(World &world, VectorCell *vector, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world, "(vector->string)", vector, vector->length(), start, end);

	StringCellBuilder builder(end - start);

	for(std::uint32_t i = start; i < end; i++)
	{
		AnyCell *member = vector->elements()[i];
		builder << cell_unchecked_cast<CharCell>(member)->unicodeChar();
	}

	return builder.result(world);
}

}
