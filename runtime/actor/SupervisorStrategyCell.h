#ifndef _LLIBY_ACTOR_SUPERVISORSTRATEGYCELL_H
#define _LLIBY_ACTOR_SUPERVISORSTRATEGYCELL_H

#include "binding/TypedProcedureCell.h"
#include "binding/SymbolCell.h"

namespace lliby
{
namespace actor
{

using SupervisorStrategyCell = TypedProcedureCell<SymbolCell*, AnyCell*>;

}
}

#endif
