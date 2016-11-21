#ifndef _LLIBY_DYNAMIC_SCHEMEEXCEPTION_H
#define _LLIBY_DYNAMIC_SCHEMEEXCEPTION_H

#include "binding/AnyCell.h"
#include "alloc/cellref.h"
#include "core/World.h"
#include "dynamic/EscapeProcedureCell.h"

namespace lliby
{
namespace dynamic
{

class SchemeException
{
public:
	SchemeException(AnyCell *object) :
		m_object(object)
	{
	}

	/**
	 * Returns the user-specified exception object for this exception
	 *
	 * For (raise) this is the argument passed to it. For (error) this is an ErrorObjectCell instance
	 */
	AnyCell *object()
	{
		return m_object;
	}

private:
	AnyCell *m_object;
};

}
}


#endif
