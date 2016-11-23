#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/ListElementCell.h"
#include "binding/ProperList.h"

#include "alloc/allocator.h"
#include "alloc/cellref.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

AnyCell* llbase_apply(World &world, TopProcedureCell *procedure, RestValues<AnyCell> *applyArgList)
{
	RestValues<AnyCell> *procArgHead;

	// Find our arguments
	if (applyArgList->empty())
	{
		// This is easy - call with no args
		procArgHead = applyArgList;
	}
	else
	{
		// Build our procedure args
		auto applyArgIt = applyArgList->begin();

		// Standalone args are zero or more args that appear before the final proper list
		auto standaloneArgCount = applyArgList->size() - 1;
		std::vector<AnyCell*> standaloneArgs(standaloneArgCount);

		for(RestValues<AnyCell>::size_type i = 0; i < standaloneArgCount; i++)
		{
			standaloneArgs[i] = *(applyArgIt++);
		}

		// Ensure the final argument is a proper list
		// This would violate our calling convention otherwise
		auto finalListHead = cell_cast<RestValues<AnyCell>>(*applyArgIt);

		if (!finalListHead)
		{
			signalError(world, ErrorCategory::Type, "Final argument to (apply) must be a proper list", {*applyArgIt});
		}

		// Root the procedure cell before allocating the argument list
		alloc::StrongRoot<TopProcedureCell> procedureRoot(world, &procedure);

		// We verified the final arg is a proper list so this must also be a proper list
		procArgHead = cell_unchecked_cast<RestValues<AnyCell>>(ListElementCell::createList(world, standaloneArgs, finalListHead));
	}

	return procedure->apply(world, procArgHead);
}

}
