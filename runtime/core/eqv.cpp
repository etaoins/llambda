#include "binding/AnyCell.h"
#include "binding/SymbolCell.h"

extern "C"
{

using namespace lliby;

bool _lliby_is_eqv(const AnyCell *cell1, const AnyCell *cell2)
{
	return cell1->isEqv(cell2);
}

bool _lliby_is_equal(const AnyCell *cell1, const AnyCell *cell2)
{
	return cell1->isEqual(cell2);
}

bool _lliby_symbol_is_eqv(const SymbolCell *cell1, const SymbolCell *cell2)
{
	// isEqv and isEqual do this internally. We need to do this manually for SymbolCell::operator==
	if (cell1 == cell2)
	{
		return true;
	}

	return *cell1 == *cell2;
}

}
