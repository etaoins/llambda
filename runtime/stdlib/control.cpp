#include "binding/ProcedureCell.h"
#include "binding/ListElementCell.h"
#include "binding/ProperList.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"
#include "alloc/StrongRef.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

DatumCell *lliby_apply(World *world, ProcedureCell *procedure, ListElementCell *argHead)
{
	ListElementCell *procArgHead;

	// Do everything inside the block so the ProperList/StrongRefRange is destructed before calling the procedure
	// This reduces the resources we use during recursive calls to (apply) and might allow the compiler to perform
	// tail call optimization
	{
		// Find our arguments
		ProperList<DatumCell> applyArgList(argHead);

		if (!applyArgList.isValid())
		{
			signalError("Non-list passed to (apply)", {argHead});
		}
		else if (applyArgList.length() == 0)
		{
			// This is easy - call with no args
			procArgHead = argHead;
		}
		else
		{
			// Build our procedure args
			auto applyArgIt = applyArgList.begin();
			
			// Standalone args are zero or more args that appear before the final final proper list
			auto standaloneArgCount = applyArgList.length() - 1;
			std::vector<DatumCell*> standaloneArgs(standaloneArgCount);

			for(auto i = 0; i < standaloneArgCount; i++)
			{
				standaloneArgs[i] = *(applyArgIt++);
			}

			// Ensure the final argument is a proper list
			// This would violate our calling convention otherwise
			auto finalListHead = datum_cast<ListElementCell>(*applyArgIt);

			if (!(finalListHead && ProperList<DatumCell>(finalListHead).isValid()))
			{
				signalError("Final argument to (apply) must be a proper list", {finalListHead});
			}

			// Reference the procedure cell before allocating the argument list
			alloc::StrongRefRange<ProcedureCell> procedureRef(&procedure, 1);	

			// We verified the final arg is a proper list so this must also be a proper list
			procArgHead = datum_unchecked_cast<ListElementCell>(ListElementCell::createList(standaloneArgs, finalListHead));
		}
	}

	return procedure->apply(world, procArgHead);
}

}
