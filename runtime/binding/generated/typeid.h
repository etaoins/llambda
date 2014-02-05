#ifndef _LLIBY_BINDING_TYPEID_H
#define _LLIBY_BINDING_TYPEID_H

/************************************************************
 * This file is generated by typegen. Do not edit manually. *
 ************************************************************/

#include <cstdint>

namespace lliby
{

enum class CellTypeId : std::uint8_t
{
	Unit = 0,
	Pair = 1,
	EmptyList = 2,
	String = 3,
	Symbol = 4,
	Boolean = 5,
	ExactInteger = 6,
	InexactRational = 7,
	Character = 8,
	Vector = 9,
	Bytevector = 10,
	Procedure = 11,
	Record = 12,
	ErrorObject = 13,
};

}

#endif
