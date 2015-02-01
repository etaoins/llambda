#ifndef _LLIBY_CORE_INIT_H
#define _LLIBY_CORE_INIT_H

namespace lliby
{

class World;

struct CommandLineArguments
{
	int argc;
	char **argv;
};

/**
 * Returns the command line arguments passed to llcore_run
 */
CommandLineArguments commandLineArguments();

}

extern "C"
{

/**
 * Initialises the runtime and starts the root world
 *
 * @param  entryPoint  Entry point to run in the root world
 * @param  argc        Command line argument count
 * @param  argv        Command line argument values
 */
void llcore_run(void (*entryPoint)(lliby::World &), int argc, char **argv);

}

#endif
