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
