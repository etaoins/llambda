#include <iostream>
#include <cassert>

#include "binding/AnyCell.h"
#include "binding/PortCell.h"
#include "binding/StringCell.h"
#include "binding/BytevectorCell.h"

#include "unicode/UnicodeChar.h"
#include "unicode/utf8.h"

#include "port/AbstractPort.h"

#include "core/error.h"

#include "util/rangeAssertions.h"
#include "util/portCellToStream.h"

using namespace lliby;

extern "C"
{

void llbase_newline(World &world, PortCell *portCell)
{
	std::ostream *portStream = portCellToOutputStream(world, portCell);
	*portStream << std::endl;
}

void llbase_write_u8(World &world, std::uint8_t value, PortCell *portCell)
{
	std::ostream *portStream = portCellToOutputStream(world, portCell);
	*portStream << static_cast<char>(value);
}

void llbase_write_char(World &world, UnicodeChar character, PortCell *portCell)
{
	if (!character.isValid())
	{
		signalError(world, ErrorCategory::Default, "(write-char) with invalid character");
	}

	utf8::EncodedChar utf8Bytes(utf8::encodeChar(character));

	std::ostream *portStream = portCellToOutputStream(world, portCell);
	portStream->write(reinterpret_cast<char*>(utf8Bytes.data), utf8Bytes.size);
}

void llbase_write_string(World &world, StringCell *stringCell, PortCell *portCell, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(write-string)", stringCell, stringCell->charLength(), start, end);

	StringCell::CharRange range = stringCell->charRange(start, end);
	assert(!range.isNull());

	// Write directly from the string's memory
	std::ostream *portStream = portCellToOutputStream(world, portCell);
	portStream->write(reinterpret_cast<const char *>(range.startPointer), range.endPointer - range.startPointer);
}

void llbase_write_bytevector(World &world, BytevectorCell *bytevectorCell, PortCell *portCell, std::int64_t start, std::int64_t end)
{
	assertSliceValid(world, "(write-bytevector)", bytevectorCell, bytevectorCell->length(), start, end);

	SharedByteArray *byteArray = bytevectorCell->byteArray();

	std::ostream *portStream = portCellToOutputStream(world, portCell);
	portStream->write(reinterpret_cast<const char *>(&byteArray->data()[start]), end - start);
}

void llbase_flush_output_port(World &world, PortCell *portCell)
{
	portCellToOutputStream(world, portCell)->flush();
}

}
