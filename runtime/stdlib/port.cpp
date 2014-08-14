#include <iostream>

#include "binding/AnyCell.h"
#include "binding/PortCell.h"

using namespace lliby;

extern "C"
{

bool lliby_is_input_port(AnyCell *obj)
{
	if (auto port = cell_cast<PortCell>(obj))
	{
		return dynamic_cast<std::istream*>(port->stream()) != nullptr;
	}
	
	return false;
}

bool lliby_is_output_port(AnyCell *obj)
{
	if (auto port = cell_cast<PortCell>(obj))
	{
		return dynamic_cast<std::ostream*>(port->stream()) != nullptr;
	}

	return false;
}

}

