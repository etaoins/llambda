#include "binding/ProcedureCell.h"
#include "binding/ListElementCell.h"
#include "binding/ProperList.h"

#include "alloc/allocator.h"
#include "alloc/WeakRef.h"
#include "alloc/RangeAlloc.h"
#include "alloc/cellref.h"

#include "dynamic/EscapeProcedureCell.h"
#include "dynamic/Continuation.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

AnyCell *lliby_apply(World &world, ProcedureCell *procedure, ListElementCell *argHead)
{
	ListElementCell *procArgHead;

	// Do everything inside the block so the ProperList/StrongRefRange is destructed before calling the procedure
	// This reduces the resources we use during recursive calls to (apply) and might allow the compiler to perform
	// tail call optimization
	{
		// Find our arguments
		ProperList<AnyCell> applyArgList(argHead);

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
			std::vector<AnyCell*> standaloneArgs(standaloneArgCount);

			for(ProperList<AnyCell>::size_type i = 0; i < standaloneArgCount; i++)
			{
				standaloneArgs[i] = *(applyArgIt++);
			}

			// Ensure the final argument is a proper list
			// This would violate our calling convention otherwise
			auto finalListHead = cell_cast<ListElementCell>(*applyArgIt);

			if (!(finalListHead && ProperList<AnyCell>(finalListHead).isValid()))
			{
				signalError(world, "Final argument to (apply) must be a proper list", {*applyArgIt});
			}

			// Reference the procedure cell before allocating the argument list
			alloc::ProcedureRefRange procedureRef(world, &procedure, 1);	

			// We verified the final arg is a proper list so this must also be a proper list
			procArgHead = cell_unchecked_cast<ListElementCell>(ListElementCell::createList(world, standaloneArgs, finalListHead));
		}
	}

	return procedure->apply(world, procArgHead);
}

AnyCell *lliby_call_with_current_continuation(World &world, ProcedureCell *proc)
{
	using dynamic::Continuation;
	using dynamic::EscapeProcedureCell;

	// This is the procedure we're calling
	alloc::ProcedureRef procRef(world, proc);
		
	// Create the escape procedure and its args
	// We build this first as there's no way to GC root a continuation at the moment
	EscapeProcedureCell *escapeProc = EscapeProcedureCell::createInstance(world, nullptr);

	// Capture the current continuation
	Continuation::CaptureResult result = Continuation::capture(world);
	
	if (result.passedValue == nullptr)
	{
		// We're the original code flow path 
		// Set the continuation on the escape proc - this will make the continuation reachable from GC
		escapeProc->setContinuation(result.continuation);
	
		// Create an argument list just contianing the escape proc
		ListElementCell *argHead = ListElementCell::createProperList(world, {escapeProc});
		
		// Invoke the procedure passing in the escape proc
		// If it returns without invoking the escape proc we'll return through here
		return procRef->apply(world, argHead);
	}
	else
	{
		// We're the result of a continuation being invoked
		return result.passedValue;
	}
}

}
