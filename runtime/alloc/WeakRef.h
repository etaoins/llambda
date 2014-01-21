#ifndef _LLIBY_ALLOC_WEAKREF_H
#define _LLIBY_ALLOC_WEAKREF_H

#include "alloc/AbstractRef.h"

namespace lliby
{
namespace alloc
{

extern CellRefList RuntimeWeakRefList;

/**
 * Holds a weaks reference to a GC managed cell
 *
 * Cells only referenced by weak references are eligible for garbage collection. However, if the cell is also reachable
 * through a strong reference any weak references will be updated in the event the cell is moved by the garbage 
 * collector.
 */
template<class T>
class WeakRef : public AbstractRef<T, RuntimeWeakRefList>
{
public:
	WeakRef() :
		AbstractRef<T, RuntimeWeakRefList>()
	{
	}

	WeakRef(T* cell) :
		AbstractRef<T, RuntimeWeakRefList>(cell)
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

