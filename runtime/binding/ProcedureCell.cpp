#include "ProcedureCell.h"

namespace lliby
{

DatumCell* ProcedureCell::invoke(ListElementCell *arguments)
{
	return m_entryPoint(this, arguments);
}

}
