#include "dynamic/EscapeProcedureCell.h"

#include "core/error.h"

namespace lliby
{
namespace dynamic
{

namespace
{
	std::uint32_t registeredClassId = ~0;

	DatumCell *procedureBody(World &world, ProcedureCell *self, ListElementCell *argHead)
	{
		signalError(world, "Generalized (call/cc) is not supported", {});
	}
}

EscapeProcedureCell* EscapeProcedureCell::createInstance(World &world)
{
	ProcedureCell *procedureCell = ProcedureCell::createInstance(world, registeredClassId, false, nullptr, &procedureBody);
	return static_cast<EscapeProcedureCell*>(procedureCell);
}

void EscapeProcedureCell::registerRecordClass()
{
	// Register our closure type so our garbage collector knows what to do
	registeredClassId = RecordLikeCell::registerRuntimeRecordClass({}); 
}

}
}
