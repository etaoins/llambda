#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/ProperList.h"
#include "binding/RestArgument.h"

#include "core/error.h"

extern "C"
{

using namespace lliby;

bool lliby_symbol_equal(SymbolCell *value1, SymbolCell *value2, RestArgument<SymbolCell> *argHead)
{
	if (*value1 != *value2)
	{
		return false;
	}
	
	ProperList<SymbolCell> properList(argHead);

	for(auto symbolCell : properList)
	{
		if (*symbolCell != *value1)
		{
			return false;
		}
	}

	return true;
}

StringCell *lliby_symbol_to_string(World &world, SymbolCell *symbol)
{
	return StringCell::fromSymbol(world, symbol);
}

SymbolCell *lliby_string_to_symbol(World &world, StringCell *string)
{
	return SymbolCell::fromString(world, string);
}

}
