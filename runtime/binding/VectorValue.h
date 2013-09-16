#ifndef _LLIBY_BINDING_VECTORVALUE_H
#define _LLIBY_BINDING_VECTORVALUE_H

#include <list>

#include "VectorLikeValue.h"

namespace lliby
{

class VectorValue : public VectorLikeValue
{
#include "generated/VectorValueMembers.h"
public:
	VectorValue(BoxedDatum **elements, std::uint32_t length) :
		VectorLikeValue(BoxedTypeId::Vector, elements, length)
	{
	}
	
	static VectorValue* fromFill(std::uint32_t length, BoxedDatum *fill = nullptr);
	static VectorValue* fromAppended(const std::list<const VectorValue*> &vectors);
	
	VectorValue* copy(std::int64_t start = 0, std::int64_t end = -1); 
	bool replace(std::uint32_t offset, const VectorValue *from, std::int64_t fromStart = 0, std::int64_t fromEnd = -1);

	bool fill(BoxedDatum *fill, std::int64_t start = 0, std::int64_t end = -1);
};

}


#endif
