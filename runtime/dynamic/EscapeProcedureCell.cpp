#include "dynamic/EscapeProcedureCell.h"

#include "core/error.h"
#include "binding/ReturnValuesList.h"
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
		// Call the continuation
		Continuation *continuation = static_cast<EscapeProcedureCell*>(procSelf)->continuation();
		continuation->resume(world, argHead);

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
