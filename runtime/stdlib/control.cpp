#include "binding/ProcedureCell.h"
#include "binding/ListElementCell.h"
#include "binding/ProperList.h"

#include "alloc/allocator.h"
#include "alloc/WeakRef.h"
#include "alloc/RangeAlloc.h"
#include "alloc/cellref.h"

#include "dynamic/EscapeProcedureCell.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

DatumCell *lliby_apply(World &world, ProcedureCell *procedure, ListElementCell *argHead)
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
			signalError(world, "Non-list passed to (apply)", {argHead});
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

			for(ProperList<DatumCell>::size_type i = 0; i < standaloneArgCount; i++)
			{
				standaloneArgs[i] = *(applyArgIt++);
			}

			// Ensure the final argument is a proper list
			// This would violate our calling convention otherwise
			auto finalListHead = datum_cast<ListElementCell>(*applyArgIt);

			if (!(finalListHead && ProperList<DatumCell>(finalListHead).isValid()))
			{
				signalError(world, "Final argument to (apply) must be a proper list", {*applyArgIt});
			}

			// Reference the procedure cell before allocating the argument list
			alloc::ProcedureRefRange procedureRef(world, &procedure, 1);	

			// We verified the final arg is a proper list so this must also be a proper list
			procArgHead = datum_unchecked_cast<ListElementCell>(ListElementCell::createList(world, standaloneArgs, finalListHead));
		}
	}

	return procedure->apply(world, procArgHead);
}

DatumCell *lliby_call_with_current_continuation(World &world, ProcedureCell *proc)
{
	// This is the procedure we're calling and it's argument head	
	alloc::ProcedureRef procRef(world, proc);
	ListElementCell *argHead;
	
	// This is the escape procedure we're passing it
	alloc::WeakRef<dynamic::EscapeProcedureCell> escapeProcRef(world);

	{
		// Create the escape procedure
		escapeProcRef.setData(
				dynamic::EscapeProcedureCell::createInstance(world)
		); 

		// Create a proper list with the escape procedure
		argHead = ListElementCell::createProperList(world, {escapeProcRef.data()});
	}

	DatumCell *returnValue = nullptr;

	try
	{
		// Invoke the procedure passing in the escape proc
		// If it returns without invoking the escape proc we'll return through here
		returnValue = procRef->apply(world, argHead);
	}
	catch (dynamic::EscapeProcedureInvokedException &e)
	{
		if (e.escapeProcedure() == escapeProcRef.data())
		{
			// The escape procedure was invoked
			returnValue = e.passedValue();
		}
		else
		{
			// This was another call/cc; invalidate ourselves and rethrow
			escapeProcRef->invalidate();
			throw;
		}
	}

	if (escapeProcRef)
	{
		// The escape procedure can no longer be used
		escapeProcRef->invalidate();
	}

	return returnValue;
}

}
