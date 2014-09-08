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
	SchemeException(AnyCell *object, EscapeProcedureCell *resumeProc = nullptr) :
		m_object(object),
		m_resumeProc(resumeProc)
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

	/**
	 * Returns the escape procedure that returns to the continuation of the (raise-continuable) that triggered this
	 * exception
	 *
	 * For normal exceptions this will return nullptr
	 */
	EscapeProcedureCell *resumeProc()
	{
		return m_resumeProc;
	}

private:
	AnyCell *m_object;
	EscapeProcedureCell *m_resumeProc;
};

}
}


#endif
