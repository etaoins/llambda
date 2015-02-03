#include "actor/ActorClosureCell.h"

#include "alloc/allocator.h"
#include "alloc/Finalizer.h"

#include "sched/Dispatcher.h"

#include "actor/ActorContext.h"
#include "actor/cloneCell.h"

#include "dynamic/SchemeException.h"
#include "dynamic/State.h"
#include "core/World.h"

namespace lliby
{
namespace actor
{

std::shared_ptr<Mailbox> ActorClosureCell::start(World &parentWorld)
{
}

}
}
