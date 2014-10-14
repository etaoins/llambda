#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/ListElementCell.h"
#include "binding/EmptyListCell.h"
#include "binding/ProperList.h"
#include "binding/RestArgument.h"

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

ReturnValuesList *lliby_apply(World &world, TopProcedureCell *procedure, RestArgument<AnyCell> *argHead)
{
	ListElementCell *procArgHead;

	// Do everything inside the block so the ProperList/StrongRefRange is destructed before calling the procedure
	// This reduces the resources we use during recursive calls to (apply) and might allow the compiler to perform
	// tail call optimization
	{
		// Find our arguments
		ProperList<AnyCell> applyArgList(argHead);

		if (applyArgList.length() == 0)
		{
			// This is easy - call with no args
			procArgHead = argHead;
		}
		else
		{
			// Build our procedure args
			auto applyArgIt = applyArgList.begin();
			
			// Standalone args are zero or more args that appear before the final proper list
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
			alloc::StrongRefRange<TopProcedureCell> procedureRef(world, &procedure, 1);	

			// We verified the final arg is a proper list so this must also be a proper list
			procArgHead = cell_unchecked_cast<ListElementCell>(ListElementCell::createList(world, standaloneArgs, finalListHead));
		}
	}

	return procedure->apply(world, procArgHead);
}

ReturnValuesList *lliby_values(RestArgument<AnyCell> *restArgHead)
{
	return restArgHead;
}

ReturnValuesList *lliby_call_with_current_continuation(World &world, TypedProcedureCell<ReturnValuesList*, ProcedureCell*> *proc)
{
	using dynamic::Continuation;
	using dynamic::EscapeProcedureCell;

	// This is the procedure we're calling
	alloc::StrongRef<TypedProcedureCell<ReturnValuesList*, ProcedureCell*>> procRef(world, proc);
		
	// Create the escape procedure and its args
	// We build this first as there's no way to GC root a continuation at the moment. This also make sure the 
	// continuation stays rooted in the resume path so we can access cont->passedValue(). Otherwise switching dynamic
	// states back to the original continuation could free the the continuation.
	alloc::StrongRef<EscapeProcedureCell> escapeRef(world, EscapeProcedureCell::createInstance(world, nullptr));

	// Capture the current continuation
	Continuation *cont = Continuation::capture(world);
	
	if (ListElementCell *passedValues = cont->takePassedValues())
	{
		// We're the result of a continuation being invoked
		return passedValues;
	}
	else
	{
		// We're the original code flow path 
		// Set the continuation on the escape proc - this will make the continuation reachable from GC
		escapeRef->setContinuation(cont);
	
		// Invoke the procedure passing in the escape proc
		// If it returns without invoking the escape proc we'll return through here
		return procRef->apply(world, escapeRef);
	}
}

ReturnValuesList *lliby_call_with_values(World &world, ThunkProcedureCell *producer, TopProcedureCell *consumerRaw)
{
	alloc::StrongRef<TopProcedureCell> consumer(world, consumerRaw);

	ReturnValuesList *values = producer->apply(world);
	return consumer->apply(world, values);
}

}
