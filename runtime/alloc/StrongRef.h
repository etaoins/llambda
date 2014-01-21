#ifndef _LLIBY_ALLOC_STRONGREF_H
#define _LLIBY_ALLOC_STRONGREF_H

#include "alloc/AbstractRef.h"
#include "alloc/CellRefList.h"

namespace lliby
{
namespace alloc
{

extern CellRefList RuntimeStrongRefList;

/**
 * Holds a strong reference to a GC managed cell
 *
 * These values and their recursively referenced values will be consider active when the garbage collector runs. Their
 * pointer values will be updated if the garbage collector moves the cell.
 */
template<class T>
class StrongRef : public AbstractRef<T, RuntimeStrongRefList>
{
public:
	StrongRef() :
		AbstractRef<T, RuntimeStrongRefList>()
	{
	}
	
	StrongRef(T* cell) :
		AbstractRef<T, RuntimeStrongRefList>(cell)
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
class StrongRefRange : public AbstractRefRange<T, RuntimeStrongRefList>
{
public:
	explicit StrongRefRange(T** cellRef, size_t cellCount) :
		AbstractRefRange<T, RuntimeStrongRefList>(cellRef, cellCount)
	{
	}
	
	explicit StrongRefRange(std::vector<T*> &cellVector) :
		AbstractRefRange<T, RuntimeStrongRefList>(cellVector)
	{
	}
};

}
}


#endif

