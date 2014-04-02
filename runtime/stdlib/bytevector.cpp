#include "binding/DatumCell.h"
#include "binding/BytevectorCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/ProperList.h"

#include "core/error.h"

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

BytevectorCell *lliby_bytevector(World &world, ListElementCell *argHead)
{
	ProperList<ExactIntegerCell> properList(argHead);
	
	if (!properList.isValid())
	{
		signalError(world, "Non-exact integer passed to (bytevector)", {argHead}); 
	}

	auto length = properList.length();
	auto newBytes = new std::uint8_t[length];
	unsigned int byteIndex = 0;

	// Fill out the new elements from the list
	for(auto element : properList)
	{
		newBytes[byteIndex++] = element->value();
	}

	// Return the new vector
	return BytevectorCell::fromOwnedData(world, newBytes, length);
}

BytevectorCell *lliby_bytevector_append(World &world, ListElementCell *argHead)
{
	ProperList<BytevectorCell> argList(argHead);
	
	if (!argList.isValid())
	{
		signalError(world, "Non-bytevector passed to (bytevector-append)", {argHead}); 
	}

	// Create a std::list
	auto bytevectorList = std::list<const BytevectorCell*>(argList.begin(), argList.end());

	// Append the vectors
	return BytevectorCell::fromAppended(world, bytevectorList);
}

}
