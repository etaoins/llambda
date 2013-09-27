#ifndef _LLIBY_BINDING_BOXEDSTRINGLIKE_H
#define _LLIBY_BINDING_BOXEDSTRINGLIKE_H

#include "BoxedDatum.h"

namespace lliby
{

class BoxedStringLike : public BoxedDatum
{
#include "generated/BoxedStringLikeMembers.h"
public:
	void finalize();

protected:
	// These are NULL safe which is required by R7RS
	bool equals(const BoxedStringLike &other) const;

	BoxedStringLike(BoxedTypeId typeId, std::uint8_t *utf8Data, std::uint32_t byteLength, std::uint32_t charLength) :
		BoxedDatum(typeId),
		m_charLength(charLength),
		m_byteLength(byteLength),
		m_utf8Data(utf8Data)
	{
	}

	void setByteLength(std::uint32_t newByteLength)
	{
		m_byteLength = newByteLength;
	}
	
	void setCharLength(std::uint32_t newCharLength)
	{
		m_charLength = newCharLength;
	}
	
	void setUtf8Data(std::uint8_t* newUtf8Data)
	{
		m_utf8Data = newUtf8Data;
	}
};

}

#endif

