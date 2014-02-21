#include "binding/ProcedureCell.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"
#include "alloc/StrongRef.h"
#include "binding/EmptyListCell.h"

#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"

using namespace lliby;

extern "C"
{

ProcedureCell *lliby_make_parameter(World &world, DatumCell *initialValue)
{
	return dynamic::ParameterProcedureCell::createInstance(world, initialValue, nullptr);
}

DatumCell *lliby_dynamic_wind(World &world, ProcedureCell *before, ProcedureCell *thunk, ProcedureCell *after)
{
	{
		// pushActiveState() can call before which can GC
		// Make sure we root thunk 
		alloc::StrongRefRange<ProcedureCell> thunkRoot(world, &thunk, 1);

		dynamic::State::pushActiveState(world, before, after);
	}
	
	alloc::StrongRef<DatumCell> thunkResult(world, thunk->apply(world, EmptyListCell::instance()));

	dynamic::State::popActiveState(world);

	return thunkResult;
}

}
