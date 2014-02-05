#include <clocale>

#include "alloc/allocator.h"
#include "core/fatal.h"
#include "core/init.h"
#include "dynamic/init.h"
#include "dynamic/SchemeException.h"
#include "dynamic/State.h"

extern "C"
{
using namespace lliby;

void lliby_init()
{
	// Use the user preferred locale
	// We assume a UTF-8 locale but don't explicitly set "UTF-8" so we still
	// get user-defined string sorting etc.
	std::setlocale(LC_ALL, "");

	alloc::init();
	dynamic::init();
}

void _lliby_launch_world(void (*entryPoint)())
{
	try
	{
		entryPoint();
	}
	catch (dynamic::SchemeException &except)
	{
		_lliby_shutdown_world();	
		_lliby_fatal("Unhandled exception", except.object());
	}
	
	_lliby_shutdown_world();	
}

void _lliby_shutdown_world()
{
	dynamic::State::popAllStates();
}

}
