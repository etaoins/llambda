#include "binding/ProcedureCell.h"
#include "binding/ListElementCell.h"
#include "binding/ProperList.h"
#include "core/fatal.h"
#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"

using namespace lliby;

extern "C"
{

DatumCell *lliby_apply(ProcedureCell *procedure, ListElementCell *argHead)
{
	// Find our arguments
	ProperList<DatumCell> applyArgList(argHead);

	if (!applyArgList.isValid())
	{
		_lliby_fatal("Non-list passed to (apply)", argHead);
	}

	if (applyArgList.length() == 0)
	{
		// This is easy - call with no args
		return procedure->apply(argHead);
	}
	
	// Build our procedure args
	auto applyArgIt = applyArgList.begin();
	
	// Standalone args are zero or more args that appear before the final final proper list
	std::list<DatumCell*> standaloneArgList;
	auto standaloneArgCount = applyArgList.length() - 1;

	while(standaloneArgCount--)
	{
		standaloneArgList.push_back(*(applyArgIt++));
	}

	// Ensure the final argument is a proper list
	// This would violate our calling convention otherwise
	auto finalListHead = datum_cast<ListElementCell>(*applyArgIt);

	if (!(finalListHead && ProperList<DatumCell>(finalListHead).isValid()))
	{
		_lliby_fatal("Final argument to (apply) must be a proper list", finalListHead);
	}

	// We verified the final arg is a proper list so this must also be a proper list
	auto procArgHead = static_cast<ListElementCell*>(ListElementCell::createList(standaloneArgList, finalListHead));

	return procedure->apply(procArgHead);
}

}
