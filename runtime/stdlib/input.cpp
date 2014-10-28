#include <iostream>

#include "binding/AnyCell.h"
#include "binding/PortCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/UnitCell.h"

#include "port/AbstractPort.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	std::istream* portCellToInputStream(World &world, PortCell *portCell)
	{
		AbstractPort *port = portCell->port();

		if (!port->isInputPort())
		{
			signalError(world, "Attempted to read from non-input port", {portCell});
		}

		if (!port->isInputPortOpen())
		{
			signalError(world, "Attempted to read from closed input port", {portCell});
		}

		return port->inputStream();
	}

	AnyCell *eofObject()
	{
		return UnitCell::instance();
	}
}

extern "C"
{

AnyCell *lliby_read_u8(World &world, PortCell *portCell)
{
	std::istream *portStream = portCellToInputStream(world, portCell);

	int readChar = portStream->get();

	if (readChar == EOF)
	{
		return eofObject();
	}
	else
	{
		return ExactIntegerCell::fromValue(world, readChar);
	}
}

AnyCell *lliby_peek_u8(World &world, PortCell *portCell)
{
	std::istream *portStream = portCellToInputStream(world, portCell);

	int peekChar = portStream->peek();

	if (peekChar == EOF)
	{
		return eofObject();
	}
	else
	{
		return ExactIntegerCell::fromValue(world, peekChar);
	}
}

}
