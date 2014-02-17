#include "dynamic/ParameterProcedureCell.h"

#include "dynamic/State.h"
#include "binding/EmptyListCell.h"
#include "alloc/StrongRef.h"

#include "core/error.h"

namespace lliby
{
namespace dynamic
{

namespace
{
	// This value should blow up the GC as a sanity check that we registered our class with registerRecordClass() at
	// startup
	std::uint32_t registeredClassId = ~0;

	DatumCell *procedureBody(World *, ProcedureCell *self, ListElementCell *argHead)
	{
		if (argHead != EmptyListCell::instance())
		{
			signalError("Parameter procedures don't accept arguments", {argHead});
		}

		// We know we're a parameter procedure because only parameter procedures have us as an entry point
		auto parameterProc = static_cast<ParameterProcedureCell*>(self);
		return State::activeState()->valueForParameter(parameterProc);
	}
}
	
ParameterProcedureCell* ParameterProcedureCell::createInstance(DatumCell *initialValueRaw, ProcedureCell *converterProcedureRaw)
{
	// Root these across the allocation of the actual procedure cell
	alloc::StrongRef<DatumCell> initialValue(initialValueRaw);
	alloc::StrongRef<ProcedureCell> converterProcedure(converterProcedureRaw);

	auto closure = static_cast<ParameterProcedureClosure*>(allocateRecordData(sizeof(ParameterProcedureClosure)));
	auto procedureCell = new ProcedureCell(registeredClassId, false, closure, &procedureBody);

	closure->initialValue = initialValue;

	if (converterProcedure == nullptr)
	{
		// No converter
		closure->converter = EmptyListCell::instance();
	}
	else
	{
		closure->converter = converterProcedure;
	}

	return static_cast<ParameterProcedureCell*>(procedureCell);
}

bool ParameterProcedureCell::isInstance(const ProcedureCell *proc)
{
	return proc->recordClassId() == registeredClassId;
}

void ParameterProcedureCell::registerRecordClass()
{
	// Register our closure type so our garbage collector knows what to do
	std::vector<size_t> offsets = {
		offsetof(ParameterProcedureClosure, initialValue),
		offsetof(ParameterProcedureClosure, converter)
	};

	registeredClassId = RecordLikeCell::registerRuntimeRecordClass(offsets);  
}

}
}
