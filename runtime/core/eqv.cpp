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
	return *cell1 == *cell2;
}

}
