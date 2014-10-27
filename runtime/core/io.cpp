#include <iostream>

#include <unistd.h>

#include "binding/PortCell.h"
#include "writer/ExternalFormDatumWriter.h"

#include "port/StandardInputPort.h"
#include "port/StandardOutputPort.h"

using namespace lliby;

extern "C"
{

PortCell *_lliby_stdout_port(World &world)
{
	return PortCell::createInstance(world, new StandardOutputPort(std::cout, STDOUT_FILENO));
}

PortCell *_lliby_stderr_port(World &world)
{
	return PortCell::createInstance(world, new StandardOutputPort(std::cerr, STDERR_FILENO));
}

PortCell *_lliby_stdin_port(World &world)
{
	return PortCell::createInstance(world, new StandardInputPort(std::cin, STDIN_FILENO));
}

void _lliby_write_stdout(AnyCell *datum)
{
	ExternalFormDatumWriter writer(std::cout);
	writer.render(datum);
}

}

