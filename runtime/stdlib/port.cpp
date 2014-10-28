#include <iostream>

#include "binding/AnyCell.h"
#include "binding/PortCell.h"

#include "port/AbstractPort.h"
#include "port/StringOutputPort.h"

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
	portCell->port()->closePort();
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

PortCell* lliby_open_output_string(World &world)
{
	return PortCell::createInstance(world, new StringOutputPort);
}

StringCell* lliby_get_output_string(World &world, PortCell *portCell)
{
	auto stringOutputPort = dynamic_cast<StringOutputPort*>(portCell->port());

	if (stringOutputPort == nullptr)
	{
		signalError(world, "Attempted (get-output-string) on non-output string port", {portCell});
	}

	return stringOutputPort->outputToStringCell(world);
}

}

