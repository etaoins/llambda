#include "util/portCellToStream.h"

#include "writer/DisplayDatumWriter.h"
#include "writer/ExternalFormDatumWriter.h"

using namespace lliby;

extern "C"
{

void llwrite_write(World &world, AnyCell *datum, PortCell *portCell)
{
	std::ostream *portStream = portCellToOutputStream(world, portCell);

	ExternalFormDatumWriter writer(*portStream);
	writer.render(datum);
}

void llwrite_display(World &world, AnyCell *datum, PortCell *portCell)
{
	std::ostream *portStream = portCellToOutputStream(world, portCell);

	DisplayDatumWriter writer(*portStream);
	writer.render(datum);
}

}
