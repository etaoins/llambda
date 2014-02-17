#ifndef _LLIBY_CORE_WORLD_H
#define _LLIBY_CORE_WORLD_H

namespace lliby
{

class World
{
};

}

extern "C"
{

void _lliby_launch_world(void (*entryPoint)(lliby::World *));

}

#endif
