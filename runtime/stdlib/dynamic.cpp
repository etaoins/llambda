#include "binding/ProcedureCell.h"

#include "alloc/allocator.h"
#include "alloc/cellref.h"
#include "binding/EmptyListCell.h"

#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"

using namespace lliby;

extern "C"
{

ProcedureCell *lliby_make_parameter(World &world, AnyCell *initialValue)
{
	return dynamic::ParameterProcedureCell::createInstance(world, initialValue, nullptr);
}

AnyCell *lliby_dynamic_wind(World &world, ProcedureCell *before, ProcedureCell *thunk, ProcedureCell *after)
{
	{
		// pushActiveState() can call before which can GC
		// Make sure we root thunk 
		alloc::ProcedureRefRange thunkRoot(world, &thunk, 1);

		dynamic::State::pushActiveState(world, before, after);
	}
	
	alloc::AnyRef thunkResult(world, thunk->apply(world, EmptyListCell::instance()));

	dynamic::State::popActiveState(world);

	return thunkResult;
}

}
