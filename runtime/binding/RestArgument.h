#ifndef _LLIBY_BINDING_RESTARGUMENT_H
#define _LLIBY_BINDING_RESTARGUMENT_H

#include "ListElementCell.h"

namespace lliby
{
	/** 
	 * Represents a rest argument passed to a variable arity function from Scheme
	 *
	 * The rest arguments are represented by a normal proper list and can freely be used as a normal ListElementCell.
	 * Our calling ABI guarantees that the rest argument will be a valid proper list of the declared member type. This
	 * allows ProperList to skip list and type validation when it is passed a RestArgument of the appropriate type.
	 */
	template<class T>
	class RestArgument : public ListElementCell
	{
	};
}

#endif
