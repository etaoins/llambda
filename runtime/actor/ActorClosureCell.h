#ifndef _LLIBY_ACTOR_ACTORCLOSURECELL_H
#define _LLIBY_ACTOR_ACTORCLOSURECELL_H

#include "binding/TypedProcedureCell.h"
#include "actor/ActorBehaviourCell.h"

namespace lliby
{
namespace actor
{

using ActorClosureCell = TypedProcedureCell<ActorBehaviourCell*>;

}
}

#endif
