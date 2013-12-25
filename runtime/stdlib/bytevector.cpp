#include "binding/DatumCell.h"
#include "binding/BytevectorCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/ProperList.h"

#include "core/fatal.h"

using namespace lliby;

extern "C"
{

BytevectorCell *lliby_make_bytevector(std::uint32_t length, std::uint8_t fill)
{
	return BytevectorCell::fromFill(length, fill);
}

std::uint32_t lliby_bytevector_length(BytevectorCell *bytevector)
{
	return bytevector->length();
}

std::uint8_t lliby_bytevector_u8_ref(const BytevectorCell *bytevector, std::uint32_t index)
{
	auto byte = bytevector->byteAt(index);

	if (byte == BytevectorCell::InvalidByte)
	{
		_lliby_fatal("Bytevector index out of bounds", bytevector);	
	}

	return byte;
}

void lliby_bytevector_u8_set(BytevectorCell *bytevector, std::uint32_t index, std::uint8_t value)
{
	if (!bytevector->setByteAt(index, value))
	{
		_lliby_fatal("Bytevector index out of bounds", bytevector);	
	}
}

BytevectorCell *lliby_bytevector(ListElementCell *argHead)
{
	ProperList<ExactIntegerCell> properList(argHead);
	
	if (!properList.isValid())
	{
		_lliby_fatal("Non-exact integer passed to (bytevector)", argHead); 
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
	return new BytevectorCell(newBytes, length);
}

BytevectorCell *lliby_bytevector_append(ListElementCell *argHead)
{
	ProperList<BytevectorCell> argList(argHead);
	
	if (!argList.isValid())
	{
		_lliby_fatal("Non-bytevector passed to (bytevector-append)", argHead); 
	}

	// Create a std::list
	auto bytevectorList = std::list<const BytevectorCell*>(argList.begin(), argList.end());

	// Append the vectors
	return BytevectorCell::fromAppended(bytevectorList);
}

}
