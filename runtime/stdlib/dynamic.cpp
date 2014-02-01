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

ProcedureCell *lliby_make_parameter(DatumCell *initialValue)
{
	return dynamic::ParameterProcedureCell::createInstance(initialValue, nullptr);
}

DatumCell *lliby_dynamic_wind(ProcedureCell *before, ProcedureCell *thunk, ProcedureCell *after)
{
	{
		// pushActiveState() can call before which can GC
		// Make sure we root thunk 
		alloc::StrongRefRange<ProcedureCell> thunkRoot(&thunk, 1);

		dynamic::State::pushActiveState(before, after);
	}
	
	alloc::StrongRef<DatumCell> thunkResult = thunk->apply(EmptyListCell::instance());

	dynamic::State::popActiveState();

	return thunkResult;
}

}
