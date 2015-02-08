#include "dynamic/EscapeProcedureCell.h"

#include <cassert>

#include "core/error.h"
#include "binding/ProperList.h"
#include "binding/ErrorObjectCell.h"
#include "dynamic/Continuation.h"
#include "dynamic/SchemeException.h"

namespace lliby
{
namespace dynamic
{

namespace
{
	RecordLikeCell::RecordClassIdType registeredClassId = ~0;
	ReturnValues<AnyCell> *procedureBody(World &world, ProcedureCell *procSelf, RestValues<AnyCell> *argHead)
	{
		assert(EscapeProcedureCell::isInstance(procSelf));

		// Call the continuation
		Continuation *continuation = static_cast<EscapeProcedureCell*>(procSelf)->continuation();
		if (!continuation->resume(world, argHead))
		{
			signalError(world, ErrorCategory::ExpiredEscapeProcedure, "Attempted to invoke an expired escape procedure");
		}

		// This code is unreachable
		__builtin_unreachable();
	}
}

EscapeProcedureCell* EscapeProcedureCell::createInstance(World &world, Continuation *continuation)
{
	TopProcedureCell *procedureCell = TopProcedureCell::createInstance(world, registeredClassId, false, continuation, &procedureBody);
	return static_cast<EscapeProcedureCell*>(procedureCell);
}

void EscapeProcedureCell::registerRecordClass()
{
	// Register our closure type so our garbage collector knows what to do
	registeredClassId = RecordLikeCell::registerRuntimeRecordClass(sizeof(Continuation), {});
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
