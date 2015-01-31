#include "actor/ActorProcedureCell.h"

using namespace lliby;

extern "C"
{

void llactor_start_actor(World &world, actor::ActorProcedureCell *actorProc)
{
	actorProc->start();
}

}
