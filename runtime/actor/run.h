#ifndef _LLIBY_ACTOR_RUN_H
#define _LLIBY_ACTOR_RUN_H

#include <memory>

#include "actor/Mailbox.h"
#include "actor/ActorClosureCell.h"

namespace lliby
{
class World;

namespace actor
{

std::shared_ptr<Mailbox> run(World &parentWorld, ActorClosureCell *closureCell);

}
}

#endif
