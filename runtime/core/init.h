#ifndef _LLIBY_CORE_INIT_H
#define _LLIBY_CORE_INIT_H

namespace lliby
{

struct CommandLineArguments
{
	int argc;
	char **argv;
};

/**
 * Returns the command line arguments passed to lliby_init
 */
CommandLineArguments commandLineArguments();

}

extern "C"
{

/**
 * Initialises the lliby runtime
 *
 * This should be called once before any calls to _lliby_launch_world
 *
 * @param  argc  Command line argument count
 * @param  argv  Command line argument values
 */
void lliby_init(int argc, char **argv);

}

#endif
