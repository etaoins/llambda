#include "dynamic/ParameterProcedureCell.h"

#include <cassert>

#include "dynamic/State.h"
#include "binding/EmptyListCell.h"

#include "core/error.h"

namespace lliby
{
namespace dynamic
{

namespace
{
	// This value should blow up the GC as a sanity check that we registered our class with registerRecordClass() at
	// startup
	RecordLikeCell::RecordClassIdType registeredClassId = ~0;

	AnyCell* procedureBody(World &world, ProcedureCell *self, RestValues<AnyCell> *argList)
	{
		assert(ParameterProcedureCell::isInstance(self));

		if (!argList->empty())
		{
			signalError(world, ErrorCategory::Arity, "Parameter procedures don't accept arguments", {argList});
		}

		// We know we're a parameter procedure because only parameter procedures have us as an entry point
		auto parameterProc = static_cast<ParameterProcedureCell*>(self);
		return State::activeState(world)->valueForParameter(parameterProc);
	}
}

ParameterProcedureCell::ParameterProcedureCell(AnyCell *initialValue) :
	TopProcedureCell(registeredClassId, true, initialValue, &procedureBody)
{
}

ParameterProcedureCell* ParameterProcedureCell::createInstance(World &world, AnyCell *initialValue)
{
	void *placement = alloc::allocateCells(world);
	return new (placement) ParameterProcedureCell(initialValue);
}

bool ParameterProcedureCell::isInstance(const ProcedureCell *proc)
{
	return proc->recordClassId() == registeredClassId;
}

void ParameterProcedureCell::registerRecordClass()
{
	// Register our closure type so our garbage collector knows what to do
	std::vector<std::size_t> offsets = { 0 };

	registeredClassId = RecordLikeCell::registerRuntimeRecordClass(sizeof(void *), offsets);
}

}
}
