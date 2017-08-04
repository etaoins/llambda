#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/ProperList.h"

#include "util/rangeAssertions.h"

extern "C"
{

using namespace lliby;

bool llbase_symbol_equal(SymbolCell *value1, SymbolCell *value2, RestValues<SymbolCell> *argHead)
{
	if (*value1 != *value2)
	{
		return false;
	}

	for(auto symbolCell : *argHead)
	{
		if (*symbolCell != *value1)
		{
			return false;
		}
	}

	return true;
}

StringCell *llbase_symbol_to_string(World &world, SymbolCell *symbol)
{
	return StringCell::fromSymbol(world, symbol);
}

SymbolCell *llbase_string_to_symbol(World &world, StringCell *string)
{
	return SymbolCell::fromString(world, string);
}

}
