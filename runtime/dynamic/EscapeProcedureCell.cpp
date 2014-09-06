#include "dynamic/EscapeProcedureCell.h"

#include "core/error.h"
#include "binding/ProperList.h"
#include "binding/RestArgument.h"
#include "binding/ReturnValuesList.h"
#include "binding/UnitCell.h"
#include "dynamic/Continuation.h"

namespace lliby
{
namespace dynamic
{

namespace
{
	std::uint32_t registeredClassId = ~0;

	ReturnValuesList *procedureBody(World &world, ProcedureCell *procSelf, ListElementCell *argHead)
	{
		ProperList<AnyCell> argList(argHead);

		if (argList.length() != 1)
		{
			signalError(world, "Escape procedures must be passed exactly one argument", {argHead});
		}

		// Call the continuation
		Continuation *continuation = static_cast<EscapeProcedureCell*>(procSelf)->continuation();
		continuation->resume(world, *argList.begin());

		// This code is unreachable
		__builtin_unreachable();
	}
}

EscapeProcedureCell* EscapeProcedureCell::createInstance(World &world, Continuation *continuation)
{
	ProcedureCell *procedureCell = ProcedureCell::createInstance(world, registeredClassId, false, continuation, &procedureBody);
	return static_cast<EscapeProcedureCell*>(procedureCell);
}

void EscapeProcedureCell::registerRecordClass()
{
	// Register our closure type so our garbage collector knows what to do
	registeredClassId = RecordLikeCell::registerRuntimeRecordClass({}); 
}

bool EscapeProcedureCell::isInstance(const ProcedureCell *proc)
{
	return proc->recordClassId() == registeredClassId;
}

Continuation* EscapeProcedureCell::continuation() const
{
	return static_cast<Continuation*>(recordData());
}

void EscapeProcedureCell::setContinuation(Continuation *continuation)
{
	setRecordData(continuation);
}

}
}
