#include <iostream>

#include "binding/DatumCell.h"
#include "binding/PortCell.h"
#include "writer/DisplayDatumWriter.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

void lliby_write(World &world, DatumCell *datum, PortCell *port)
{
	std::ostream *portStream;

	if (!(portStream = dynamic_cast<std::ostream*>(port->stream())))
	{
		signalError(world, "Attempted to write to non-output port", {port});	
	}

	ExternalFormDatumWriter writer(*portStream);
	writer.render(datum);
}

void lliby_display(World &world, DatumCell *datum, PortCell *port)
{
	std::ostream *portStream;
	
	if (!(portStream = dynamic_cast<std::ostream*>(port->stream())))
	{
		signalError(world, "Attempted to write to non-output port", {port});	
	}

	DisplayDatumWriter writer(*portStream);
	writer.render(datum);
}

void lliby_newline(World &world, PortCell *port)
{
	std::ostream *portStream;
	
	if (!(portStream = dynamic_cast<std::ostream*>(port->stream())))
	{
		signalError(world, "Attempted to write to non-output port", {port});	
	}

	*portStream << std::endl;
}

}
