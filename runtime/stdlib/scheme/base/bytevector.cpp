#include "binding/AnyCell.h"
#include "binding/BytevectorCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/ProperList.h"
#include "binding/StringCell.h"

#include "unicode/utf8/InvalidByteSequenceException.h"
#include "core/error.h"

#include "util/rangeAssertions.h"
#include "util/utf8ExceptionToSchemeError.h"

using namespace lliby;

extern "C"
{

BytevectorCell *llbase_make_bytevector(World &world, std::int64_t length, std::uint8_t fill)
{
	assertLengthValid(world, "(make-bytevector)", "bytevector length", BytevectorCell::maximumLength(), length);

	auto bytevector = BytevectorCell::fromFill(world, length, fill);

	if (bytevector == nullptr)
	{
		signalError(world, ErrorCategory::OutOfMemory, "Out of memory in (make-bytevector)");
	}

	return bytevector;
}

std::int64_t llbase_bytevector_length(BytevectorCell *bytevector)
{
	return bytevector->length();
}

std::uint8_t llbase_bytevector_u8_ref(World &world, BytevectorCell *bytevector, std::int64_t index)
{
	assertIndexValid(world, "(bytevector-u8-ref)", bytevector, bytevector->length(), index);

	return bytevector->byteAt(index);
}

void llbase_bytevector_u8_set(World &world, BytevectorCell *bytevector, std::int64_t index, std::uint8_t value)
{
	if (bytevector->isGlobalConstant())
	{
		signalError(world, ErrorCategory::MutateLiteral, "(bytevector-set!) on bytevector literal", {bytevector});
	}

	assertIndexValid(world, "(bytevector-u8-set!)", bytevector, bytevector->length(), index);

	bytevector->setByteAt(index, value);
}

BytevectorCell *llbase_bytevector(World &world, RestValues<ExactIntegerCell> *argList)
{
	auto length = argList->size();
	SharedByteArray *byteArray = SharedByteArray::createInstance(length);
	unsigned int byteIndex = 0;

	// Fill out the new elements from the list
	for(auto element : *argList)
	{
		byteArray->data()[byteIndex++] = element->value();
	}

	// Return the new vector
	return BytevectorCell::withByteArray(world, byteArray, length);
}

BytevectorCell *llbase_bytevector_append(World &world, RestValues<BytevectorCell> *argList)
{
	// Create a std::list
	auto bytevectorList = std::list<const BytevectorCell*>(argList->begin(), argList->end());

	// Append the vectors
	return BytevectorCell::fromAppended(world, bytevectorList);
}

BytevectorCell *llbase_string_to_utf8(World &world, StringCell *string, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(string->utf8)", string, string->charLength(), start, end);
	return string->toUtf8Bytevector(world, start, end);
}

StringCell *llbase_utf8_to_string(World &world, BytevectorCell *bytevector, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(utf8->string)", bytevector, bytevector->length(), start, end);
	assertLengthValid(world, "(utf8->string)", "string byte length", StringCell::maximumByteLength(), end - start);

	try
	{
		return bytevector->utf8ToString(world, start, end);
	}
	catch (const utf8::InvalidByteSequenceException &e)
	{
		utf8ExceptionToSchemeError(world, "(utf8->string)", e, bytevector);
	}
}

BytevectorCell *llbase_bytevector_copy(World &world, BytevectorCell *bytevector, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(bytevector-copy)", bytevector, bytevector->length(), start, end);
	return bytevector->copy(world, start, end);
}

void llbase_bytevector_mutating_copy(World &world, BytevectorCell *to, std::int64_t at, BytevectorCell *from, std::int64_t start, std::int64_t end)
{
	if (to->isGlobalConstant())
	{
		signalError(world, ErrorCategory::MutateLiteral, "(bytevector-copy!) on bytevector literal", {to});
	}

	assertSliceValid(world, "(bytevector-copy!)", from, from->length(), start, end);
	assertSliceValid(world, "(bytevector-copy!)", to, to->length(), at, at + (end - start));

	to->replace(at, from, start, end);
}

}
