#include <iostream>

#include "binding/DatumCell.h"
#include "binding/PortCell.h"

using namespace lliby;

extern "C"
{

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

