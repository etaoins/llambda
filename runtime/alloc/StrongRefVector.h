#ifndef _LLIBY_ALLOC_STRONGREFVECTOR_H
#define _LLIBY_ALLOC_STRONGREFVECTOR_H

#include "alloc/AbstractRefVector.h"
#include "core/World.h"

namespace lliby
{
namespace alloc
{

template<class T>
class StrongRefVector : public AbstractRefVector<T>
{
public:
	template<typename... Arguments>
	explicit StrongRefVector(World &world, Arguments... parameters) :
		AbstractRefVector<T>(&world.strongRoots(), parameters...)
	{
	}
};

}
}

#endif
