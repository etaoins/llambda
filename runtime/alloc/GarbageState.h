#ifndef _LLIBY_ALLOC_GARBAGESTATE_H
#define _LLIBY_ALLOC_GARBAGESTATE_H

#include <cstdint>

namespace lliby
{

enum class GarbageState : std::uint8_t
{
	/**
	 * Cells allocated dynamically from the garbage collector
	 *
	 * Allocated cells may be unreferenced; only a garbage collection can determine if a cell is reachable.
	 */
	AllocatedCell = 0,
	
	/** 
	 * Cells compiled in to the executables' read-only data at compile time
	 *
	 * This includes both preconstructed cells from the runtime (empty list, booleans, etc) and constants included by
	 * the compiler. By definitiion they can only reference other GlobalConstant cells
	 */
	GlobalConstant = 1,

	/**
	 * Cells that have been relocated during garbage collection 
	 *
	 * The forwarding cell will contain a pointer to the new location. These cells are used internally by the garbage
	 * collector and are not visible when the GC is not running.
	 */
	ForwardingCell = 2,

	/**
	 * Cell to terminate a heap segment
	 *
	 * These occur at the end of every non-terminal heap segment to point to the next heap segment.
	 *
	 * These aren't actual datum cells; they're only used internally by the allocator
	 */
	SegmentTerminator = 3,
	
	/**
	 * Cell to terminate a heap
	 *
	 * This occurs once at the end of the heap. This is placed by the allocator to mark the end of the heap before
	 * collecting garbage.
	 *
	 * These aren't actual datum cells; they're only used internally by the allocator
	 */
	HeapTerminator = 4,

	MaximumGarbageState = HeapTerminator
};

}

#endif

