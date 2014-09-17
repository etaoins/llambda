#include "binding/ProcedureCell.h"

#include "alloc/allocator.h"
#include "alloc/cellref.h"
#include "binding/EmptyListCell.h"
#include "binding/TypedProcedureCell.h"

#include "dynamic/State.h"
#include "dynamic/ParameterProcedureCell.h"

using namespace lliby;

extern "C"
{

ReturnValuesList *lliby_dynamic_wind(World &world, TopProcedureCell *before, TopProcedureCell *thunk, TopProcedureCell *after)
{
	{
		// pushActiveState() can call before which can GC
		// Make sure we root thunk 
		alloc::StrongRefRange<TopProcedureCell> thunkRoot(world, &thunk, 1);

		dynamic::State::pushActiveState(world, before, after);
	}
	
	alloc::ListElementRef thunkResult(world, thunk->apply(world, EmptyListCell::instance()));

	dynamic::State::popActiveState(world);

	return thunkResult;
}

}
