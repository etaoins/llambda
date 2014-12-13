#include <vector>

#include "binding/ListElementCell.h"
#include "binding/ProperList.h"

using namespace lliby;

extern "C"
{

AnyCell *lllist_cons_star(World &world, AnyCell *headValue, RestValues<AnyCell> *tailValues)
{
	const auto elementCount = tailValues->size() + 1;

	if (elementCount == 1)
	{
		// This is easy
		return headValue;
	}

	std::vector<AnyCell*> nonTerminalElements(elementCount - 1);

	nonTerminalElements[0] = headValue;

	auto tailArgIt = tailValues->begin();
	for(RestValues<AnyCell>::size_type i = 1; i < (elementCount - 1); i++)
	{
		nonTerminalElements[i] = *(tailArgIt++);
	}

	return ListElementCell::createList(world, nonTerminalElements, *tailArgIt);
}

}
