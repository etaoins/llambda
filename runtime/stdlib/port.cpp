#include <iostream>

#include "binding/DatumCell.h"
#include "binding/PortCell.h"

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

bool lliby_is_input_port(DatumCell *obj)
{
	if (auto port = datum_cast<PortCell>(obj))
	{
		return dynamic_cast<std::istream*>(port->stream()) != nullptr;
	}
	
	return false;
}

bool lliby_is_output_port(DatumCell *obj)
{
	if (auto port = datum_cast<PortCell>(obj))
	{
		return dynamic_cast<std::ostream*>(port->stream()) != nullptr;
	}

	return false;
}

}

