#ifndef _LLIBY_CORE_INIT_H
#define _LLIBY_CORE_INIT_H

extern "C"
{

void lliby_init();

void _lliby_launch_world(void (*entryPoint)());
void _lliby_shutdown_world();

}

#endif
