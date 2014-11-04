#include <iostream>

#include "binding/AnyCell.h"
#include "binding/PortCell.h"
#include "binding/TypedProcedureCell.h"

#include "port/AbstractPort.h"
#include "port/StringOutputPort.h"
#include "port/BufferInputPort.h"
#include "port/BytevectorOutputPort.h"

#include "alloc/WeakRef.h"

#include "util/SharedByteArray.h"

#include "core/error.h"

using namespace lliby;

using CallWithPortProcedureCell = TypedProcedureCell<ReturnValuesList*, PortCell*>;

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

PortCell* lliby_open_output_bytevector(World &world)
{
	return PortCell::createInstance(world, new BytevectorOutputPort);
}

BytevectorCell* lliby_get_output_bytevector(World &world, PortCell *portCell)
{
	auto bytevectorOutputPort = dynamic_cast<BytevectorOutputPort*>(portCell->port());

	if (bytevectorOutputPort == nullptr)
	{
		signalError(world, "Attempted (get-output-bytevector) on non-output bytevector port", {portCell});
	}

	return bytevectorOutputPort->outputToBytevectorCell(world);
}

PortCell* lliby_open_input_string(World &world, StringCell *string)
{
	std::string inputString(string->toUtf8StdString());

	return PortCell::createInstance(world, new BufferInputPort(inputString));
}

PortCell* lliby_open_input_bytevector(World &world, BytevectorCell *bytevector)
{
	SharedByteArray *byteArray = bytevector->byteArray();
	std::string inputString(reinterpret_cast<const char *>(byteArray->data()), bytevector->length());

	return PortCell::createInstance(world, new BufferInputPort(inputString));
}

ReturnValuesList *lliby_call_with_port(World &world, PortCell *portCell, CallWithPortProcedureCell *thunk)
{
	alloc::WeakRef<PortCell> portRef(world, portCell);

	ReturnValuesList *returnValues = thunk->apply(world, portCell);

	// If the port hasn't been GCed then close it
	if (portRef)
	{
		portRef->port()->closePort();
	}

	return returnValues;

}

}

