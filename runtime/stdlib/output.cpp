#include <iostream>

#include "binding/BoxedDatum.h"
#include "writer/ExternalFormDatumWriter.h"

using namespace lliby;

extern "C"
{

void lliby_write(BoxedDatum *datum)
{
	// XXX: Use the current port
	auto writer = new ExternalFormDatumWriter(std::cout);
	writer->render(datum);
}

}
