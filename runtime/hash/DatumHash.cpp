#include "hash/DatumHash.h"

#include <cmath>
#include <cassert>

#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/ProcedureCell.h"
#include "binding/CharCell.h"
#include "binding/BytevectorCell.h"
#include "binding/EmptyListCell.h"
#include "binding/PairCell.h"
#include "binding/VectorCell.h"
#include "binding/MailboxCell.h"
#include "binding/UnitCell.h"
#include "binding/RecordCell.h"
#include "binding/EofObjectCell.h"
#include "binding/PortCell.h"
#include "binding/ErrorObjectCell.h"

namespace
{
using HashResultType = lliby::DatumHash::HashResultType;

std::uint32_t djb2StringHash(const std::uint8_t *data, std::size_t length)
{
	std::uint32_t h = 5381;
	for(std::size_t i = 0; i < length; i++)
	{
		h = (h * 33) + data[i];
	}

	return h;
}

template<typename T>
HashResultType convertToHashResultType(T value)
{
	if (sizeof(T) == sizeof(HashResultType))
	{
		return *reinterpret_cast<HashResultType*>(&value);
	}
	else if ((sizeof(T) == 8) && (sizeof(HashResultType) == 4))
	{
		auto intValue = *reinterpret_cast<std::uint64_t*>(&value);
		return (intValue >> 33) ^ intValue;
	}

	assert(false);
}

HashResultType combineHash(HashResultType seed, HashResultType value)
{
	return seed ^ value + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

}


namespace lliby
{

DatumHash::HashResultType DatumHash::operator()(AnyCell *datum) const
{
	if (auto stringCell = cell_cast<StringCell>(datum))
	{
		return djb2StringHash(stringCell->constUtf8Data(), stringCell->byteLength()) ^ 0x50b778f2;
	}
	else if (auto symbolCell = cell_cast<SymbolCell>(datum))
	{
		return djb2StringHash(symbolCell->constUtf8Data(), symbolCell->byteLength()) ^ 0x636720ec;
	}
	else if (auto booleanCell = cell_cast<BooleanCell>(datum))
	{
		if (booleanCell->value())
		{
			return 0x1d418aff;
		}
		else
		{
			return 0x5898b4bf;
		}
	}
	else if (auto exactIntCell = cell_cast<ExactIntegerCell>(datum))
	{
		return convertToHashResultType(exactIntCell->value()) ^ 0x392ed847;
	}
	else if (auto flonumCell = cell_cast<FlonumCell>(datum))
	{
		auto floatValue = flonumCell->value();

		// There are many bit representations of NaN but they all are considered equal by Llambda
		if (std::isnan(floatValue))
		{
			return 0x44645abf;
		}

		return convertToHashResultType(floatValue) ^ 0x8bc111e4;
	}
	else if (auto procCell = cell_cast<ProcedureCell>(datum))
	{
		if (!procCell->capturesVariables())
		{
			return convertToHashResultType(procCell->entryPoint()) ^ 0x1be73aa7;
		}

		return convertToHashResultType(procCell) ^ 0xf181f9bf;
	}
	else if (auto charCell = cell_cast<CharCell>(datum))
	{
		return convertToHashResultType(charCell->unicodeChar().codePoint()) ^ 0x90a39786;
	}
	else if (auto bvCell = cell_cast<BytevectorCell>(datum))
	{
		return djb2StringHash(bvCell->byteArray()->data(), bvCell->length()) ^ 0x2bd5dbe9;
	}
	else if (EmptyListCell::isInstance(datum))
	{
		return 0x698100ad;
	}
	else if (auto pairCell = cell_cast<PairCell>(datum))
	{
		auto carHash = (*this)(pairCell->car());
		auto cdrHash = (*this)(pairCell->cdr());

		return combineHash(carHash, cdrHash) ^ 0xa8685aa0;
	}
	else if (auto vectorCell = cell_cast<VectorCell>(datum))
	{
		HashResultType runningHash = 0xb45537fa;

		for(VectorCell::LengthType i = 0; i < vectorCell->length(); i++)
		{
			runningHash = combineHash(runningHash, (*this)(vectorCell->elements()[i]));
		}

		return runningHash;
	}
	else if (auto mailboxCell = cell_cast<MailboxCell>(datum))
	{
		return convertToHashResultType(mailboxCell->lockedMailbox().get()) ^ 0x7f3bc1fa;
	}
	else if (UnitCell::isInstance(datum))
	{
		return 0x54c77308;
	}
	else if (auto recordCell = cell_cast<RecordCell>(datum))
	{
		// There isn't much entropy in the pointer's lower bits; mix in the class ID which should help
		return convertToHashResultType(recordCell) ^ recordCell->recordClassId() ^ 0x46f38277;
	}
	else if (EofObjectCell::isInstance(datum))
	{
		return 0x28be88a0;
	}
	else if (auto portCell = cell_cast<PortCell>(datum))
	{
		return convertToHashResultType(portCell) ^ 0x3982978b;
	}
	else if (auto errObjCell = cell_cast<ErrorObjectCell>(datum))
	{
		// Use the error category to add some entropy to the lower bits
		return convertToHashResultType(errObjCell) ^
			static_cast<HashResultType>(errObjCell->category()) ^
			0x969cc581;
	}
	else
	{
		assert(false);
		return 0;
	}
}

}
