#ifndef _LLIBY_DYNAMIC_SCHEMEEXCEPTION_H
#define _LLIBY_DYNAMIC_SCHEMEEXCEPTION_H

#include "binding/AnyCell.h"
#include "alloc/cellref.h"
#include "core/World.h"

namespace lliby
{
namespace dynamic
{

class SchemeException
{
public:
	SchemeException(World &world, AnyCell *object) :
		m_object(world, object)
	{
	}

	AnyCell *object()
	{
		return m_object;
	}

private:
	alloc::AnyRef m_object;
};

}
}


#endif
