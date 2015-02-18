#ifndef _LLIBY_BINDING_PORTCELL_H
#define _LLIBY_BINDING_PORTCELL_H

#include "AnyCell.h"

namespace lliby
{
class World;
class AbstractPort;

class PortCell : public AnyCell
{
#include "generated/PortCellMembers.h"
public:
	explicit PortCell(AbstractPort *port) :
		AnyCell(CellTypeId::Port),
		m_port(port)
	{
	}

	PortCell(AbstractPort *port, GarbageState gcState) :
		AnyCell(CellTypeId::Port, gcState),
		m_port(port)
	{
	}

	static PortCell* createInstance(World &world, AbstractPort *port);

	void finalizePort();
};

}


#endif
