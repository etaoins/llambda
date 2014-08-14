#include <iostream>

#include "binding/PortCell.h"
#include "writer/ExternalFormDatumWriter.h"

using namespace lliby;

extern "C"
{

PortCell *_lliby_stdout_port(World &world)
{
	return PortCell::createInstance(world, &std::cout, false);
}

PortCell *_lliby_stderr_port(World &world)
{
	return PortCell::createInstance(world, &std::cerr, false);
}

PortCell *_lliby_stdin_port(World &world)
{
	return PortCell::createInstance(world, &std::cin, false);
}

void _lliby_write_stdout(AnyCell *datum)
{
	ExternalFormDatumWriter writer(std::cout);
	writer.render(datum);
}

}

