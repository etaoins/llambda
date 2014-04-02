#include <clocale>

#include "core/init.h"
#include "dynamic/init.h"

#include "alloc/allocator.h"
#include "alloc/DynamicMemoryBlock.h"

extern "C"
{
using namespace lliby;

void lliby_init()
{
#if !defined(NDEBUG) && defined(__APPLE__)
	// XXX: Valgrind 3.9.0 on Mac OS X 10.9 will return a NULL pointer for the first mmap()
	// This confuses DynamicMemoryBlock greatly
	// Create a throwaway memory block so subsequent allocations succeed
	new alloc::DynamicMemoryBlock(4096);
#endif

	// Use the user preferred locale
	// We assume a UTF-8 locale but don't explicitly set "UTF-8" so we still
	// get user-defined string sorting etc.
	std::setlocale(LC_ALL, "");
	dynamic::init();

	// Start the allocator
	alloc::initGlobal();
}

}
