#ifndef _LLIBY_BINDING_PORTCELL_H
#define _LLIBY_BINDING_PORTCELL_H

#include "DatumCell.h"
#include <iostream>

namespace lliby
{
class World;

class PortCell : public DatumCell
{
#include "generated/PortCellMembers.h"
public:
	static PortCell* createInstance(World &world, std::ios* stream, bool isOwned = true);
	
	void finalizePort();

protected:
	PortCell(std::ios *stream, bool isOwned = true) :
		DatumCell(CellTypeId::Port),
		m_isOwned(isOwned),
		m_stream(stream)
	{
	}
};
	
}


#endif
