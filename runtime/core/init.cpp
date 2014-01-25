#include <clocale>

#include "alloc/allocator.h"
#include "dynamic/init.h"

extern "C"
{

void lliby_init()
{
	using namespace lliby;

	// Use the user preferred locale
	// We assume a UTF-8 locale but don't explicitly set "UTF-8" so we still
	// get user-defined string sorting etc.
	std::setlocale(LC_ALL, "");

	alloc::init();
	dynamic::init();
}

}
