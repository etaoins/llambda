#include <iostream>

#include "binding/DatumCell.h"
#include "writer/DisplayDatumWriter.h"

using namespace lliby;

extern "C"
{

void lliby_write(DatumCell *datum)
{
	// XXX: Use the current port
	ExternalFormDatumWriter writer(std::cout);
	writer.render(datum);
}

void lliby_display(DatumCell *datum)
{
	DisplayDatumWriter writer(std::cout);
	writer.render(datum);
}

void lliby_newline(DatumCell *datum)
{
	std::cout << std::endl;
}

}
