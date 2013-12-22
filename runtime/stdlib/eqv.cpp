#include "binding/DatumCell.h"

extern "C"
{

using namespace lliby;

bool lliby_is_eqv(const DatumCell *cell1, const DatumCell *cell2)
{
	return cell1->isEqv(cell2);
}

}
