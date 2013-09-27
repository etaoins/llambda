#ifndef _LLIBY_BINDING_BOXEDVECTORLIKE_H
#define _LLIBY_BINDING_BOXEDVECTORLIKE_H

#include "BoxedDatum.h"

namespace lliby
{

class BoxedVectorLike : public BoxedDatum
{
#include "generated/BoxedVectorLikeMembers.h"
public:
	void finalize();
	
	BoxedDatum* elementAt(std::uint32_t offset) const
	{
		if (offset >= length())
		{
			return nullptr;
		}
		
		return elements()[offset];
	}

	bool setElementAt(std::uint32_t offset, BoxedDatum *value)
	{
		if (offset >= length())
		{
			return false;
		}

		elements()[offset] = value;

		return true;
	}

protected:
	BoxedVectorLike(BoxedTypeId typeId, BoxedDatum **elements, std::uint32_t length) :
		BoxedDatum(typeId),
		m_length(length),
		m_elements(elements)
	{
	}
};

}

#endif
