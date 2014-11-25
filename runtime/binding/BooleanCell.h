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
	
	static BooleanCell* falseInstance()
	{
		return const_cast<BooleanCell*>(&llcore_false_value);
	}

	static BooleanCell* trueInstance()
	{
		return const_cast<BooleanCell*>(&llcore_true_value);
	}
};

}

#endif

