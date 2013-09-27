#ifndef _LLIBY_BINDING_BOXEDVECTOR_H
#define _LLIBY_BINDING_BOXEDVECTOR_H

#include <list>

#include "BoxedVectorLike.h"

namespace lliby
{

class BoxedVector : public BoxedVectorLike
{
#include "generated/BoxedVectorMembers.h"
public:
	BoxedVector(BoxedDatum **elements, std::uint32_t length) :
		BoxedVectorLike(BoxedTypeId::Vector, elements, length)
	{
	}
	
	static BoxedVector* fromFill(std::uint32_t length, BoxedDatum *fill = nullptr);
	static BoxedVector* fromAppended(const std::list<const BoxedVector*> &vectors);
	
	BoxedVector* copy(std::int64_t start = 0, std::int64_t end = -1); 
	bool replace(std::uint32_t offset, const BoxedVector *from, std::int64_t fromStart = 0, std::int64_t fromEnd = -1);

	bool fill(BoxedDatum *fill, std::int64_t start = 0, std::int64_t end = -1);
};

}


#endif
