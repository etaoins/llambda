#include "util/portCellToStream.h"

#include "port/AbstractPort.h"
#include "core/error.h"

namespace lliby
{

std::ostream* portCellToOutputStream(World &world, PortCell *portCell)
{
	AbstractPort *port = portCell->port();

	if (!port->isOutputPort())
	{
		signalError(world, ErrorCategory::Default, "Attempted to write to non-output port", {portCell});
	}

	if (!port->isOutputPortOpen())
	{
		signalError(world, ErrorCategory::Default, "Attempted to write to closed output port", {portCell});
	}

	return port->outputStream();
}

std::istream* portCellToInputStream(World &world, PortCell *portCell)
{
	AbstractPort *port = portCell->port();

	if (!port->isInputPort())
	{
		signalError(world, ErrorCategory::Default, "Attempted to read from non-input port", {portCell});
	}

	if (!port->isInputPortOpen())
	{
		signalError(world, ErrorCategory::Default, "Attempted to read from closed input port", {portCell});
	}

	return port->inputStream();
}

}
