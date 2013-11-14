#ifndef _LLIBY_BINDING_BOXEDVECTOR_H
#define _LLIBY_BINDING_BOXEDVECTOR_H

#include "BoxedDatum.h"
#include <list>

namespace lliby
{

class BoxedVector : public BoxedDatum
{
#include "generated/BoxedVectorMembers.h"
public:
	BoxedVector(BoxedDatum **elements, std::uint32_t length) :
		BoxedDatum(BoxedTypeId::Vector),
		m_length(length),
		m_elements(elements)
	{
	}
	
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
	static BoxedVector* fromFill(std::uint32_t length, BoxedDatum *fill = nullptr);
	static BoxedVector* fromAppended(const std::list<const BoxedVector*> &vectors);
	
	BoxedVector* copy(std::int64_t start = 0, std::int64_t end = -1); 
	bool replace(std::uint32_t offset, const BoxedVector *from, std::int64_t fromStart = 0, std::int64_t fromEnd = -1);

	bool fill(BoxedDatum *fill, std::int64_t start = 0, std::int64_t end = -1);
};

}


#endif
