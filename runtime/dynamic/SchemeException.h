#ifndef _LLIBY_DYNAMIC_SCHEMEEXCEPTION_H
#define _LLIBY_DYNAMIC_SCHEMEEXCEPTION_H

#include "binding/DatumCell.h"
#include "alloc/StrongRef.h"
#include "core/World.h"

namespace lliby
{
namespace dynamic
{

class SchemeException
{
public:
	SchemeException(World &world, DatumCell *object) :
		m_object(world, object)
	{
	}

	DatumCell *object()
	{
		return m_object;
	}

private:
	alloc::StrongRef<DatumCell> m_object;
};

}
}


#endif
