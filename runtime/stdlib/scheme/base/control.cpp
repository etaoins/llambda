#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/ListElementCell.h"
#include "binding/ProperList.h"

#include "alloc/allocator.h"
#include "alloc/cellref.h"

#include "dynamic/EscapeProcedureCell.h"
#include "dynamic/Continuation.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

ReturnValues<AnyCell> *llbase_apply(World &world, TopProcedureCell *procedure, RestValues<AnyCell> *applyArgList)
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

ReturnValues<AnyCell>* llbase_values(RestValues<AnyCell> *restArgHead)
{
	return restArgHead;
}

ReturnValues<AnyCell>* llbase_call_with_current_continuation(World &world, TypedProcedureCell<ReturnValues<AnyCell>*, ProcedureCell*> *proc)
{
	using dynamic::Continuation;
	using dynamic::EscapeProcedureCell;

	// This is the procedure we're calling
	alloc::StrongRef<TypedProcedureCell<ReturnValues<AnyCell>*, ProcedureCell*>> procRef(world, proc);

	// Create the escape procedure and its args
	// We build this first as there's no way to GC root a continuation at the moment. This also make sure the 
	// continuation stays rooted in the resume path so we can access cont->passedValue(). Otherwise switching dynamic
	// states back to the original continuation could free the the continuation.
	alloc::StrongRef<EscapeProcedureCell> escapeRef(world, EscapeProcedureCell::createInstance(world, nullptr));

	// Capture the current continuation
	Continuation *cont = Continuation::capture(world);
	
	if (ProperList<AnyCell> *passedValues = cont->takePassedValues())
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

ReturnValues<AnyCell> *llbase_call_with_values(World &world, ThunkProcedureCell *producer, TopProcedureCell *consumerRaw)
{
	alloc::StrongRef<TopProcedureCell> consumer(world, consumerRaw);

	ReturnValues<AnyCell> *values = producer->apply(world);
	return consumer->apply(world, values);
}

}
