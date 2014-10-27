#include "PortCell.h"

#include "alloc/allocator.h"

#include "port/AbstractPort.h"

namespace lliby
{

PortCell* PortCell::createInstance(World &world, AbstractPort *port)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) PortCell(port);
}

void PortCell::finalizePort()
{
	delete m_port;
}

}
