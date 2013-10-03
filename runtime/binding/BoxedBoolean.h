#ifndef _LLIBY_BINDING_BOXEDBOOLEAN_H
#define _LLIBY_BINDING_BOXEDBOOLEAN_H

#include "BoxedSingleton.h"
#include "core/constinstances.h"

namespace lliby
{

class BoxedBoolean : public BoxedSingleton<BoxedDatum>
{
#include "generated/BoxedBooleanMembers.h"
public:
	explicit BoxedBoolean(bool value) :
		BoxedSingleton(BoxedTypeId::Boolean),
		m_value(value)
	{
	}

	static const BoxedBoolean* instanceForValue(bool value)
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
	
	static const BoxedBoolean* falseInstance()
	{
		return &lliby_false_value;
	}

	static const BoxedBoolean* trueInstance()
	{
		return &lliby_true_value;
	}
};

}

#endif

