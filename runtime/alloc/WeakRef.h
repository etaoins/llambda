#ifndef _LLIBY_ALLOC_WEAKREF_H
#define _LLIBY_ALLOC_WEAKREF_H

#include "alloc/AbstractRef.h"
#include "core/World.h"

namespace lliby
{
namespace alloc
{

/**
 * Holds a weaks reference to a GC managed cell
 *
 * Cells only referenced by weak references are eligible for garbage collection. However, if the cell is also reachable
 * through a strong reference any weak references will be updated in the event the cell is moved by the garbage
 * collector.
 */
template<class T>
class WeakRef : public AbstractRef<T>
{
public:
	WeakRef(World &world) :
		AbstractRef<T>(&world.weakRoots())
	{
	}

	WeakRef(World &world, T* cell) :
		AbstractRef<T>(&world.weakRoots(), cell)
	{
	}

	WeakRef& operator=(T* newCell)
	{
		this->setData(newCell);
		return *this;
	}
};

}
}

#endif

