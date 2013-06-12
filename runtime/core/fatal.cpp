#include <iostream>
#include <unistd.h>

#include "fatal.h"

extern "C"
{
	void lliby_fatal(const char *message)
	{
		std::cerr << message << std::endl;
		exit(-1);
		__builtin_unreachable();
	}
}
