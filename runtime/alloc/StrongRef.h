#ifndef _LLIBY_ALLOC_STRONGREF_H
#define _LLIBY_ALLOC_STRONGREF_H

#include "alloc/AbstractRef.h"
#include "core/World.h"

namespace lliby
{
namespace alloc
{

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
		AbstractRef<T>(&world.strongRefs)
	{
	}
	
	StrongRef(World &world, T* cell) :
		AbstractRef<T>(&world.strongRefs, cell)
	{
	}
	
	StrongRef& operator=(T* newCell)
	{
		this->setData(newCell);
		return *this;
	}
};

/**
 * Declares a range of externally allocated GC root pointers
 *
 * It is the reponsibility of the caller to ensure the external range remains valid for the lifetime of the
 * StrongRefRange instance
 */
template<class T>
class StrongRefRange : public AbstractRefRange<T>
{
public:
	explicit StrongRefRange(World &world, T** cellRef, size_t cellCount) :
		AbstractRefRange<T>(&world.strongRefs, cellRef, cellCount)
	{
	}
	
	explicit StrongRefRange(World &world, std::vector<T*> &cellVector) :
		AbstractRefRange<T>(&world.strongRefs, cellVector)
	{
	}
};

}
}


#endif

