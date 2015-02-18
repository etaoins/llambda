#include "binding/AnyCell.h"
#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/ErrorObjectCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"

#include "alloc/cellref.h"

#include "dynamic/State.h"
#include "dynamic/EscapeProcedureCell.h"
#include "dynamic/Continuation.h"
#include "dynamic/SchemeException.h"

using namespace lliby;

extern "C"
{

using HandlerProcedureCell = TypedProcedureCell<ReturnValues<AnyCell>*, AnyCell*>;

ReturnValues<AnyCell>* llbase_with_exception_handler(World &world, HandlerProcedureCell *handlerRaw, ThunkProcedureCell *thunk)
{
	// Root our exception handler
	alloc::StrongRef<HandlerProcedureCell> handler(world, handlerRaw);

	// Keep track of our dynamic state
	alloc::DynamicStateRef expectedStateRef(world, world.activeStateCell());

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
		ReturnValues<AnyCell> *handlerResult = handler->apply(world, except.object());

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

ReturnValues<AnyCell> *llbase_guard_kernel(World &world, HandlerProcedureCell *guardAuxProcRaw, ThunkProcedureCell *thunk)
{
	alloc::StrongRef<HandlerProcedureCell> guardAuxProc(world, guardAuxProcRaw);
	alloc::DynamicStateRef expectedStateRef(world, world.activeStateCell());

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

void llbase_raise(World &world, AnyCell *obj)
{
	throw dynamic::SchemeException(obj);
}

ReturnValues<AnyCell>* llbase_raise_continuable(World &world, AnyCell *obj)
{
	using dynamic::EscapeProcedureCell;
	using dynamic::Continuation;

	alloc::StrongRef<EscapeProcedureCell> resumeRef(world, EscapeProcedureCell::createInstance(world, nullptr));

	Continuation *cont = Continuation::capture(world);

	if (ProperList<AnyCell> *passedValues = cont->takePassedValues())
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

void llbase_error(World &world, StringCell *message, RestValues<AnyCell> *irritants)
{
	llbase_raise(world, ErrorObjectCell::createInstance(world, message, irritants));
}

StringCell* llbase_error_object_message(ErrorObjectCell *errorObject)
{
	return errorObject->message();
}

ProperList<AnyCell>* llbase_error_object_irritants(ErrorObjectCell *errorObject)
{
	return errorObject->irritants();
}

bool llbase_is_file_error(AnyCell *obj)
{
	if (auto errorObj = cell_cast<ErrorObjectCell>(obj))
	{
		return errorObj->category() == ErrorCategory::File;
	}

	return false;
}

bool llbase_is_read_error(AnyCell *obj)
{
	if (auto errorObj = cell_cast<ErrorObjectCell>(obj))
	{
		return errorObj->category() == ErrorCategory::Read;
	}

	return false;
}

}
