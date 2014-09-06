#include "binding/AnyCell.h"
#include "binding/ProcedureCell.h"
#include "binding/EmptyListCell.h"
#include "binding/ListElementCell.h"
#include "binding/ErrorObjectCell.h"

#include "alloc/cellref.h"

#include "dynamic/State.h"
#include "dynamic/EscapeProcedureCell.h"
#include "dynamic/Continuation.h"
#include "dynamic/SchemeException.h"

#include <iostream>

using namespace lliby;

extern "C"
{

ReturnValuesList* lliby_with_exception_handler(World &world, ProcedureCell *handlerRaw, ProcedureCell *thunk)
{
	// Root our exception handler
	alloc::ProcedureRef handler(world, handlerRaw);

	// Keep track of our dynamic state
	alloc::DynamicStateRef expectedStateRef(world, world.activeStateCell);

	try
	{
		return thunk->apply(world, EmptyListCell::instance());
	}
	catch (dynamic::SchemeException &except)
	{
		ListElementCell *argHead = ListElementCell::createProperList(world, {except.object()});

		// Call the handler in the dynamic environment (raise) was in
		// This is required by R7RS for reasons mysterious to me
		AnyCell *handlerResult = handler->apply(world, argHead);

		// Is this a resumable exception?
		if (except.resumeProc() != nullptr)
		{
			except.resumeProc()->continuation()->resume(world, handlerResult);
		}
		
		// Now switch to the state we were in before re-raising the exception
		dynamic::State::switchStateCell(world, expectedStateRef);

		throw except;
	}
}

void lliby_raise(World &world, AnyCell *obj)
{
	throw dynamic::SchemeException(world, obj);
}

AnyCell* lliby_raise_continuable(World &world, AnyCell *obj)
{
	using dynamic::EscapeProcedureCell;
	using dynamic::Continuation;

	alloc::StrongRef<EscapeProcedureCell> resumeRef(world, EscapeProcedureCell::createInstance(world, nullptr));

	Continuation *cont = Continuation::capture(world);

	if (AnyCell *passedValue = cont->takePassedValue())
	{
		// The exception handler resumed us
		// XXX: Support multiple values
		return ListElementCell::createProperList(world, {passedValue});
	}
	else
	{
		// We're in the original capture path - throw an exception!
		resumeRef->setContinuation(cont);
		throw dynamic::SchemeException(world, obj, resumeRef);
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
