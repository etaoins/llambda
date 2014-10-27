#include <iostream>

#include "binding/AnyCell.h"
#include "binding/PortCell.h"

#include "port/AbstractPort.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

bool lliby_is_input_port(AnyCell *obj)
{
	if (auto portCell = cell_cast<PortCell>(obj))
	{
		return portCell->port()->isInputPort();
	}

	return false;
}

bool lliby_is_output_port(AnyCell *obj)
{
	if (auto portCell = cell_cast<PortCell>(obj))
	{
		return portCell->port()->isOutputPort();
	}

	return false;
}

bool lliby_is_input_port_open(PortCell *portCell)
{
	return portCell->port()->isInputPortOpen();
}

bool lliby_is_output_port_open(PortCell *portCell)
{
	return portCell->port()->isOutputPortOpen();
}

void lliby_close_port(PortCell *portCell)
{
	AbstractPort *port = portCell->port();

	if (port->isInputPortOpen())
	{
		port->closeInputPort();
	}

	if (port->isOutputPortOpen())
	{
		port->closeOutputPort();
	}
}

void lliby_close_input_port(World &world, PortCell *portCell)
{
	AbstractPort *port = portCell->port();

	if (!port->isInputPort())
	{
		signalError(world, "Attempted (close-input-port) on non-input port", {portCell});
	}

	port->closeInputPort();
}

void lliby_close_output_port(World &world, PortCell *portCell)
{
	AbstractPort *port = portCell->port();

	if (!port->isOutputPort())
	{
		signalError(world, "Attempted (close-output-port) on non-output port", {portCell});
	}

	port->closeOutputPort();
}

}

