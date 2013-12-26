#include "ProcedureCell.h"

namespace lliby
{

DatumCell* ProcedureCell::apply(ListElementCell *arguments)
{
	return m_entryPoint(this, arguments);
}

}
