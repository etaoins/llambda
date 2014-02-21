#include "ProcedureCell.h"

namespace lliby
{

DatumCell* ProcedureCell::apply(World &world, ListElementCell *arguments)
{ 
	return m_entryPoint(world, this, arguments);
}

}
