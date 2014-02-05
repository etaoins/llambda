#include "binding/ProperList.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"

#include "core/error.h"

extern "C"
{

using namespace lliby;

bool lliby_symbol_equal(SymbolCell *value1, SymbolCell *value2, ListElementCell *argHead)
{
	if (*value1 != *value2)
	{
		return false;
	}
	
	ProperList<SymbolCell> properList(argHead);

	if (!properList.isValid())
	{
		signalError("Non-symbol passed to (symbol=?)", {argHead});
	}

	for(auto symbolCell : properList)
	{
		if (*symbolCell != *value1)
		{
			return false;
		}
	}

	return true;
}

StringCell *lliby_symbol_to_string(const SymbolCell *symbol)
{
	return StringCell::fromSymbol(symbol);
}

SymbolCell *lliby_string_to_symbol(const StringCell *string)
{
	return string->toSymbol();
}

}
