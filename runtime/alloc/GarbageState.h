#ifndef _LLIBY_ALLOC_GARBAGESTATE_H
#define _LLIBY_ALLOC_GARBAGESTATE_H

#include <cstdint>

namespace lliby
{

enum class GarbageState : std::uint8_t
{
	/** 
	 * Cells compiled in to the executables read-only data at compile time
	 *
	 * This includes both preconstructed cells from the runtime (empty list, booleans, etc) and constants included by
	 * the compiler. By definitiion they can only reference other GlobalConstant cells
	 */
	GlobalConstant = 0,

	/**
	 * Cells allocated dynamically from the garbage collector
	 *
	 * Allocated cells may be unreferenced; only a garbage collection can determine if a cell is reachable.
	 */
	AllocatedCell = 1,

	/**
	 * Cells that have been relocated during garbage collection 
	 *
	 * The forwarding cell will contain a pointer to the new location. These cells are used internally by the garbage
	 * collector and are not visible when the GC is not running.
	 */
	ForwardingCell = 2
};

}

#endif

