#include "binding/ProcedureCell.h"
#include "binding/ProperList.h"
#include "core/fatal.h"

using namespace lliby;

extern "C"
{

DatumCell *lliby_apply(ProcedureCell *procedure, ListElementCell *args)
{
	// Make sure the args are a proper list
	// For efficiency the generated procedure trampolines assume the arg list
	// is proper
	if (!ProperList<DatumCell>(args).isValid())
	{
		_lliby_fatal("Non-list passed to (apply)", args);
	}
	
	return procedure->apply(args);
}

}
