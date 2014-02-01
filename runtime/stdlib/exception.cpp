#include "binding/DatumCell.h"
#include "binding/ProcedureCell.h"
#include "binding/EmptyListCell.h"
#include "binding/ListElementCell.h"

#include "alloc/StrongRef.h"
#include "dynamic/State.h"
#include "dynamic/SchemeException.h"

#include <iostream>

using namespace lliby;

extern "C"
{

DatumCell* lliby_with_exception_handler(ProcedureCell *handlerRaw, ProcedureCell *thunk)
{
	// Root our exception handler
	alloc::StrongRef<ProcedureCell> handler(handlerRaw);

	// Keep track of our dynamic state
	dynamic::State *expectedState = dynamic::State::activeState();

	try
	{
		return thunk->apply(EmptyListCell::instance());
	}
	catch (dynamic::SchemeException &except)
	{
		ListElementCell *argHead = ListElementCell::createProperList({except.object()});

		// Call the handler in the dynamic environment (raise) was in
		// This is required by R7RS for reasons mysterious to me
		handler->apply(argHead);
		
		// Now switch to the state we were in before re-raising the exception
		dynamic::State::switchState(expectedState);

		throw except;
	}
}

void lliby_raise(DatumCell *obj)
{
	throw dynamic::SchemeException(obj);
}

}
