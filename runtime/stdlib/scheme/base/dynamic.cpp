#include "binding/ProcedureCell.h"

#include "alloc/allocator.h"
#include "alloc/cellref.h"
#include "binding/TypedProcedureCell.h"

#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"

using namespace lliby;

extern "C"
{

AnyCell* llbase_dynamic_wind(World &world, ThunkProcedureCell *before, ThunkProcedureCell *thunk, ThunkProcedureCell *after)
{
	{
		// pushActiveState() can call before which can GC
		// Make sure we root thunk
		alloc::StrongRoot<ThunkProcedureCell> thunkRoot(world, &thunk);

		dynamic::State::pushActiveState(world, before, after);
	}

	alloc::StrongRef<AnyCell> thunkResult(world, thunk->apply(world));

	dynamic::State::popActiveState(world);

	return thunkResult;
}

}
