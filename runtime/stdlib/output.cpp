#include <iostream>

#include "binding/AnyCell.h"
#include "binding/PortCell.h"
#include "writer/DisplayDatumWriter.h"

#include "port/AbstractPort.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	std::ostream* portCellToOutputStream(World &world, PortCell *portCell)
	{
		AbstractPort *port = portCell->port();

		if (!port->isOutputPort())
		{
			signalError(world, "Attempted to write to non-output port", {portCell});
		}

		if (!port->isOutputPortOpen())
		{
			signalError(world, "Attempted to write to closed output port", {portCell});
		}

		return port->outputStream();
	}
}

extern "C"
{

void lliby_write(World &world, AnyCell *datum, PortCell *portCell)
{
	std::ostream *portStream = portCellToOutputStream(world, portCell);

	ExternalFormDatumWriter writer(*portStream);
	writer.render(datum);
}

void lliby_display(World &world, AnyCell *datum, PortCell *portCell)
{
	std::ostream *portStream = portCellToOutputStream(world, portCell);

	DisplayDatumWriter writer(*portStream);
	writer.render(datum);
}

void lliby_newline(World &world, PortCell *portCell)
{
	std::ostream *portStream = portCellToOutputStream(world, portCell);
	*portStream << std::endl;
}

}
