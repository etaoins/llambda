#include <iostream>

#include "binding/DatumCell.h"
#include "writer/ExternalFormDatumWriter.h"

using namespace lliby;

extern "C"
{

void lliby_write(DatumCell *datum)
{
	// XXX: Use the current port
	auto writer = new ExternalFormDatumWriter(std::cout);
	writer->render(datum);

	std::cout << std::endl;
}

}
