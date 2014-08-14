#include "binding/AnyCell.h"

extern "C"
{

using namespace lliby;

bool lliby_is_eqv(const AnyCell *cell1, const AnyCell *cell2)
{
	return cell1->isEqv(cell2);
}

bool lliby_is_equal(const AnyCell *cell1, const AnyCell *cell2)
{
	return cell1->isEqual(cell2);
}

}
