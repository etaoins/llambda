#include "dynamic/EscapeProcedureCell.h"

#include "core/error.h"
#include "binding/ProperList.h"

namespace lliby
{
namespace dynamic
{

namespace
{
	std::uint32_t registeredClassId = ~0;

	AnyCell *procedureBody(World &world, ProcedureCell *procSelf, ListElementCell *argHead)
	{
		EscapeProcedureCell *escapeSelf = static_cast<EscapeProcedureCell*>(procSelf);
		ProperList<AnyCell> argList(argHead);

		if (!argList.isValid() || (argList.length() != 1))
		{
			signalError(world, "Escape procedures must be passed exactly one argument", {argHead});
		}

		if (escapeSelf->isInvalidated())
		{
			// This is a current implementation limitation of llambda
			signalError(world, "Invoked escape procedure that has fallen out of scope", {});
		}
		else
		{
			throw EscapeProcedureInvokedException(world, escapeSelf, *argList.begin());
		}
	}
}

EscapeProcedureCell* EscapeProcedureCell::createInstance(World &world)
{
	auto closure = static_cast<EscapeProcedureClosure*>(allocateRecordData(sizeof(EscapeProcedureClosure)));

	ProcedureCell *procedureCell = ProcedureCell::createInstance(world, registeredClassId, false, closure, &procedureBody);
	closure->invalidated = false;	

	return static_cast<EscapeProcedureCell*>(procedureCell);
}

void EscapeProcedureCell::registerRecordClass()
{
	// Register our closure type so our garbage collector knows what to do
	registeredClassId = RecordLikeCell::registerRuntimeRecordClass({}); 
}

void EscapeProcedureCell::invalidate()
{
	static_cast<EscapeProcedureClosure*>(recordData())->invalidated = true;
}
	
bool EscapeProcedureCell::isInvalidated() const
{
	return static_cast<EscapeProcedureClosure*>(recordData())->invalidated;
}

}
}
