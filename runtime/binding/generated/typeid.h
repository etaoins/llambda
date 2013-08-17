#ifndef _LLIBY_BINDING_TYPEID_H
#define _LLIBY_BINDING_TYPEID_H

/*****************************************************************
 * This file is generated by gen-types.py. Do not edit manually. *
 *****************************************************************/

#include <cstdint>

namespace lliby
{

enum class BoxedTypeId : std::uint16_t
{
	Unspecific = 0,
	Pair = 1,
	EmptyList = 2,
	String = 3,
	Symbol = 4,
	Boolean = 5,
	ExactInteger = 6,
	InexactRational = 7,
	Character = 8,
	ByteVector = 9,
	Procedure = 10,
	Vector = 32768,
	Closure = 32769,
};

}

#endif