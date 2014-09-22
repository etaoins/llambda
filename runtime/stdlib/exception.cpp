#include "binding/AnyCell.h"
#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/EmptyListCell.h"
#include "binding/ListElementCell.h"
#include "binding/ReturnValuesList.h"
#include "binding/ErrorObjectCell.h"
#include "binding/BooleanCell.h"

#include "alloc/cellref.h"

#include "dynamic/State.h"
#include "dynamic/EscapeProcedureCell.h"
#include "dynamic/Continuation.h"
#include "dynamic/SchemeException.h"

using namespace lliby;

extern "C"
{

using ThunkProcedureCell = TypedProcedureCell<ReturnValuesList*>;
using HandlerProcedureCell = TypedProcedureCell<ReturnValuesList*, AnyCell*>;

ReturnValuesList* lliby_with_exception_handler(World &world, HandlerProcedureCell *handlerRaw, ThunkProcedureCell *thunk)
{
	// Root our exception handler
	alloc::StrongRef<HandlerProcedureCell> handler(world, handlerRaw);

	// Keep track of our dynamic state
	alloc::DynamicStateRef expectedStateRef(world, world.activeStateCell);

	try
	{
		return thunk->apply(world);
	}
	catch (dynamic::SchemeException &except)
	{
		// GC root the exception's information
		// We need to do this to be able to reconstruct the exception in the case that our continuation is captured
		// inside the Scheme exception handler. C++ exceptions are allocated outside of the normal stack so they
		// aren't copied as part of the (call/cc) capture process
		alloc::AnyRef objectRef(world, except.object());
		alloc::StrongRef<dynamic::EscapeProcedureCell> resumeProcRef(world, except.resumeProc());

		// Call the handler in the dynamic environment (raise) was in
		// This is required by R7RS for reasons mysterious to me
		ReturnValuesList *handlerResult = handler->apply(world, except.object());

		// Is this a resumable exception?
		if (resumeProcRef != nullptr)
		{
			resumeProcRef->continuation()->resume(world, handlerResult);
		}
		
		// Now switch to the state we were in before re-raising the exception
		dynamic::State::switchStateCell(world, expectedStateRef);

		// Reconstruct the original exception and rethrow
		throw dynamic::SchemeException(objectRef, resumeProcRef);
	}
}

ReturnValuesList *_lliby_guard_kernel(World &world, HandlerProcedureCell *guardAuxProcRaw, ThunkProcedureCell *thunk) 
{
	alloc::StrongRef<HandlerProcedureCell> guardAuxProc(world, guardAuxProcRaw);
	alloc::DynamicStateRef expectedStateRef(world, world.activeStateCell);

	try
	{
		return thunk->apply(world); 
	}
	catch (dynamic::SchemeException &except)
	{
		alloc::AnyRef objectRef(world, except.object());

		// Switch to the guard's dynamic state
		dynamic::State::switchStateCell(world, expectedStateRef);

		// Call our guard-aux procedure
		// This will re-throw if no match is encountered
		return guardAuxProc->apply(world, objectRef);
	}
}

void lliby_raise(World &world, AnyCell *obj)
{
	throw dynamic::SchemeException(obj);
}

ReturnValuesList* lliby_raise_continuable(World &world, AnyCell *obj)
{
	using dynamic::EscapeProcedureCell;
	using dynamic::Continuation;

	alloc::StrongRef<EscapeProcedureCell> resumeRef(world, EscapeProcedureCell::createInstance(world, nullptr));

	Continuation *cont = Continuation::capture(world);

	if (ListElementCell *passedValues = cont->takePassedValues())
	{
		// The exception handler resumed us
		return passedValues; 
	}
	else
	{
		// We're in the original capture path - throw an exception!
		resumeRef->setContinuation(cont);
		throw dynamic::SchemeException(obj, resumeRef);
	}
}

void lliby_error(World &world, StringCell *message, ListElementCell *irritants)
{
	lliby_raise(world, ErrorObjectCell::createInstance(world, message, irritants));
}

StringCell* lliby_error_object_message(ErrorObjectCell *errorObject)
{
	return errorObject->message();
}

ListElementCell* lliby_error_object_irritants(ErrorObjectCell *errorObject)
{
	return errorObject->irritants();
}

}
