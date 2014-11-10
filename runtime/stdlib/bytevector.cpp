#include "binding/AnyCell.h"
#include "binding/BytevectorCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/ProperList.h"
#include "binding/StringCell.h"

#include "unicode/utf8/InvalidByteSequenceException.h"
#include "core/error.h"

#include "util/assertSliceValid.h"
#include "util/utf8ExceptionToSchemeError.h"

using namespace lliby;

extern "C"
{

BytevectorCell *lliby_make_bytevector(World &world, std::uint32_t length, std::uint8_t fill)
{
	return BytevectorCell::fromFill(world, length, fill);
}

std::uint32_t lliby_bytevector_length(BytevectorCell *bytevector)
{
	return bytevector->length();
}

std::uint8_t lliby_bytevector_u8_ref(World &world, BytevectorCell *bytevector, std::uint32_t index)
{
	auto byte = bytevector->byteAt(index);

	if (byte == BytevectorCell::InvalidByte)
	{
		signalError(world, "Bytevector index out of bounds", {bytevector});	
	}

	return byte;
}

void lliby_bytevector_u8_set(World &world, BytevectorCell *bytevector, std::uint32_t index, std::uint8_t value)
{
	if (bytevector->isGlobalConstant())
	{
		signalError(world, "(bytevector-set!) on bytevector literal", {bytevector});	
	}

	if (!bytevector->setByteAt(index, value))
	{
		signalError(world, "Bytevector index out of bounds", {bytevector});	
	}
}

BytevectorCell *lliby_bytevector(World &world, ProperList<ExactIntegerCell> *argList)
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

BytevectorCell *lliby_bytevector_append(World &world, ProperList<BytevectorCell> *argList)
{
	// Create a std::list
	auto bytevectorList = std::list<const BytevectorCell*>(argList->begin(), argList->end());

	// Append the vectors
	return BytevectorCell::fromAppended(world, bytevectorList);
}

BytevectorCell *lliby_string_to_utf8(World &world, StringCell *string, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world, "(string->utf8)", string, string->charLength(), start, end);
	return string->toUtf8Bytevector(world, start, end);
}

StringCell *lliby_utf8_to_string(World &world, BytevectorCell *bytevector, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world, "(utf8->string)", bytevector, bytevector->length(), start, end);

	try
	{
		return bytevector->utf8ToString(world, start, end);
	}
	catch (const utf8::InvalidByteSequenceException &e)
	{
		utf8ExceptionToSchemeError(world, "(utf8->string)", e, bytevector);
	}
}

BytevectorCell *lliby_bytevector_copy(World &world, BytevectorCell *bytevector, std::uint32_t start, std::uint32_t end)
{
	return bytevector->copy(world, start, end);
}

void lliby_bytevector_mutating_copy(World &world, BytevectorCell *to, std::uint32_t at, BytevectorCell *from, std::uint32_t start, std::uint32_t end)
{
	assertSliceValid(world, "(bytevector-copy!)", from, from->length(), start, end);
	assertSliceValid(world, "(bytevector-copy!)", to, to->length(), at, at + (end - start));

	to->replace(at, from, start, end);
}

}
