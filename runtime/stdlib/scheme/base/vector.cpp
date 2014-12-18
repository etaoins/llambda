#include "binding/AnyCell.h"
#include "binding/VectorCell.h"
#include "binding/ProperList.h"
#include "binding/StringCell.h"
#include "binding/CharCell.h"

#include "core/error.h"
#include "core/World.h"

#include "unicode/UnicodeChar.h"

#include "util/assertIndexValid.h"
#include "util/assertSliceValid.h"
#include "util/StringCellBuilder.h"

using namespace lliby;

extern "C"
{

VectorCell *llbase_make_vector(World &world, std::uint32_t length, AnyCell *fill)
{
	return VectorCell::fromFill(world, length, fill);
}

std::uint32_t llbase_vector_length(VectorCell *vector)
{
	return vector->length();
}

AnyCell* llbase_vector_ref(World &world, VectorCell *vector, std::int64_t index)
{
	assertIndexValid(world,"(vector-ref)", vector, vector->length(), index);

	return vector->elementAt(index);
}

void llbase_vector_set(World &world, VectorCell *vector, std::int64_t index, AnyCell *obj)
{
	assertIndexValid(world,"(vector-set!)", vector, vector->length(), index);

	if (vector->isGlobalConstant())
	{
		signalError(world, ErrorCategory::MutateLiteral, "(vector-set!) on vector literal", {vector});
	}

	vector->setElementAt(index, obj);
}

VectorCell *llbase_vector(World &world, RestValues<AnyCell> *argList)
{
	const auto length = argList->size();

	auto newElements = new AnyCell*[length];
	std::copy(argList->begin(), argList->end(), newElements);

	// Return the new vector
	return VectorCell::fromElements(world, newElements, length);
}

VectorCell *llbase_vector_append(World &world, RestValues<VectorCell> *argList)
{
	// Create a std::vector
	auto vectorElements = std::vector<const VectorCell*>(argList->begin(), argList->end());

	// Append the vectors
	return VectorCell::fromAppended(world, vectorElements);
}

ProperList<AnyCell> *llbase_vector_to_list(World &world, VectorCell *vectorCell, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world,"(vector->list)", vectorCell, vectorCell->length(), start, end);

	AnyCell **startPointer = vectorCell->elements() + start;
	AnyCell **endPointer = vectorCell->elements() + end;

	std::vector<AnyCell*> vectorElements(startPointer, endPointer);
	return ProperList<AnyCell>::create(world, vectorElements);
}

VectorCell *llbase_vector_copy(World &world, VectorCell *sourceVector, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(vector-copy)", sourceVector, sourceVector->length(), start, end);

	return sourceVector->copy(world, start, end);
}

void llbase_vector_mutating_copy(World &world, VectorCell *to, std::int64_t at, VectorCell *from, std::int64_t start, std::int64_t end)
{
	if (to->isGlobalConstant())
	{
		signalError(world, ErrorCategory::MutateLiteral, "(vector-copy!) on vector literal", {to});
	}

	assertSliceValid(world, "(vector-copy!)", from, from->length(), start, end);
	assertSliceValid(world, "(vector-copy!)", to, to->length(), at, at + (end - start));

	to->replace(at, from, start, end);
}

void llbase_vector_mutating_fill(World &world, VectorCell *vector, AnyCell *fill, std::int64_t start, std::int64_t end)
{
	if (vector->isGlobalConstant())
	{
		signalError(world, ErrorCategory::MutateLiteral, "(vector-fill!) on vector literal", {vector});
	}

	assertSliceValid(world, "(vector-fill!)", vector, vector->length(), start, end);

	vector->fill(fill, start, end);
}

VectorCell* llbase_string_to_vector(World &world, StringCell *string, std::int64_t start, std::int64_t end)
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

StringCell *llbase_vector_to_string(World &world, VectorCell *vector, std::int64_t start, std::int64_t end)
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
