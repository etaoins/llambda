#include "PortCell.h"

#include "alloc/allocator.h"

namespace lliby
{

PortCell* PortCell::createInstance(World &world, std::ios *stream, bool isOwned)
{
	void *cellPlacement = alloc::allocateCells(world);
	return new (cellPlacement) PortCell(stream, isOwned);
}

void PortCell::finalizePort()
{
	if (isOwned())
	{
		delete m_stream;
	}
}

}
