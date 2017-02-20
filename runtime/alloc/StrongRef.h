#ifndef _LLIBY_ALLOC_STRONGREF_H
#define _LLIBY_ALLOC_STRONGREF_H

#include "alloc/AbstractRef.h"
#include "core/World.h"

namespace lliby
{
namespace alloc
{

/**
 * Declares a range of externally allocated GC root pointers
 *
 * It is the responsibility of the caller to ensure the external range remains valid for the lifetime of the StrongRoot
 * instance
 */
template<class T>
class StrongRoot : public AbstractRoot<T>
{
public:
	StrongRoot(World &world, T** cellRef, std::size_t cellCount = 1) :
		AbstractRoot<T>(&world.strongRoots(), cellRef, cellCount)
	{
	}
};

/**
 * Holds a strong reference to a GC managed cell
 *
 * These values and their recursively referenced values will be consider active when the garbage collector runs. Their
 * pointer values will be updated if the garbage collector moves the cell.
 */
template<class T>
class StrongRef : public AbstractRef<T>
{
public:
	StrongRef(World &world) :
		AbstractRef<T>(&world.strongRoots())
	{
	}

	StrongRef(World &world, T* cell) :
		AbstractRef<T>(&world.strongRoots(), cell)
	{
	}

	StrongRef& operator=(T* newCell)
	{
		this->setData(newCell);
		return *this;
	}
};

}
}

#endif
