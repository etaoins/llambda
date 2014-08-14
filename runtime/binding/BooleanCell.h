#ifndef _LLIBY_BINDING_BOOLEANCELL_H
#define _LLIBY_BINDING_BOOLEANCELL_H

#include "PreconstructedValue.h"
#include "core/constinstances.h"

namespace lliby
{

class BooleanCell : public PreconstructedValue<AnyCell>
{
#include "generated/BooleanCellMembers.h"
public:
	explicit BooleanCell(bool value) :
		PreconstructedValue(CellTypeId::Boolean),
		m_value(value)
	{
	}

	static const BooleanCell* instanceForValue(bool value)
	{
		if (value)
		{
			return trueInstance();
		}
		else
		{
			return falseInstance();
		}
	}
	
	static const BooleanCell* falseInstance()
	{
		return &lliby_false_value;
	}

	static const BooleanCell* trueInstance()
	{
		return &lliby_true_value;
	}
};

}

#endif

