#include "hash/DatumHash.h"

#include <cmath>
#include <cassert>

#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/BooleanCell.h"
#include "binding/IntegerCell.h"
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
#include "binding/HashMapCell.h"

#include "hash/DatumHashTree.h"
#include "hash/SharedByteHash.h"
#include "classmap/RecordClassMap.h"

namespace
{
using ResultType = lliby::DatumHash::ResultType;

template<typename T>
ResultType convertToResultType(T value)
{
	if (sizeof(T) == sizeof(ResultType))
	{
		return *reinterpret_cast<ResultType*>(&value);
	}
	else if ((sizeof(T) == 8) && (sizeof(ResultType) == 4))
	{
		auto intValue = *reinterpret_cast<std::uint64_t*>(&value);
		return (intValue >> 33) ^ intValue;
	}

	assert(false);
}

ResultType combineHash(ResultType seed, ResultType value)
{
	return seed ^ value + 0x9e3779b9 + (seed << 6) + (seed >> 2);
}

}


namespace lliby
{

DatumHash::ResultType DatumHash::operator()(AnyCell *datum) const
{
	if (auto stringCell = cell_cast<StringCell>(datum))
	{
		return stringCell->sharedByteHash() ^ 0x50b778f2;
	}
	else if (auto symbolCell = cell_cast<SymbolCell>(datum))
	{
		return symbolCell->sharedByteHash() ^ 0x636720ec;
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
	else if (auto integerCell = cell_cast<IntegerCell>(datum))
	{
		return convertToResultType(integerCell->value()) ^ 0x392ed847;
	}
	else if (auto flonumCell = cell_cast<FlonumCell>(datum))
	{
		auto floatValue = flonumCell->value();

		// There are many bit representations of NaN but they all are considered equal by Llambda
		if (std::isnan(floatValue))
		{
			return 0x44645abf;
		}

		return convertToResultType(floatValue) ^ 0x8bc111e4;
	}
	else if (auto procCell = cell_cast<ProcedureCell>(datum))
	{
		return convertToResultType(procCell->entryPoint()) ^ hashRecordLike(procCell) ^ 0x466e8954;
	}
	else if (auto charCell = cell_cast<CharCell>(datum))
	{
		return convertToResultType(charCell->unicodeChar().codePoint()) ^ 0x90a39786;
	}
	else if (auto bvCell = cell_cast<BytevectorCell>(datum))
	{
		return bvCell->byteArray()->hashValue(bvCell->length()) ^ 0x2bd5dbe9;
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
		ResultType runningHash = 0xb45537fa;

		for(VectorCell::LengthType i = 0; i < vectorCell->length(); i++)
		{
			runningHash = combineHash(runningHash, (*this)(vectorCell->elements()[i]));
		}

		return runningHash;
	}
	else if (auto mailboxCell = cell_cast<MailboxCell>(datum))
	{
		return convertToResultType(mailboxCell->lockedMailbox().get()) ^ 0x7f3bc1fa;
	}
	else if (UnitCell::isInstance(datum))
	{
		return 0x54c77308;
	}
	else if (auto recordCell = cell_cast<RecordCell>(datum))
	{
		return hashRecordLike(recordCell) ^ 0xb851fff0;
	}
	else if (EofObjectCell::isInstance(datum))
	{
		return 0x28be88a0;
	}
	else if (auto portCell = cell_cast<PortCell>(datum))
	{
		return convertToResultType(portCell->port()) ^ 0x3982978b;
	}
	else if (auto errObjCell = cell_cast<ErrorObjectCell>(datum))
	{
		auto messageHash = (*this)(errObjCell->message());
		auto irritantsHash = (*this)(errObjCell->irritants());

		return combineHash(messageHash, irritantsHash) ^ static_cast<ResultType>(errObjCell->category());
	}
	else if (auto hashMapCell = cell_cast<HashMapCell>(datum))
	{
		auto runningHash = 0x8eb51105;

		DatumHashTree::every(hashMapCell->datumHashTree(), [&] (AnyCell *key, AnyCell *value, DatumHash::ResultType hashValue)
		{
			// Note that we don't combine the running hash so the order of iteration does not matter
			runningHash ^= combineHash(hashValue, (*this)(value));
			return true;
		});

		return runningHash;
	}
	else
	{
		assert(false);
		return 0;
	}
}

ResultType DatumHash::hashRecordLike(RecordLikeCell *recordLike) const
{
	SharedByteHash byteHasher;
	ResultType h = recordLike->recordClassId();

	const RecordClassMap *classMap = recordLike->classMap();
	auto dataBase = static_cast<const std::uint8_t*>(recordLike->dataBasePointer());

	std::size_t currentByte = 0;
	for(std::uint32_t i = 0; i < classMap->offsetCount; i++)
	{
		std::size_t nextCellOffset = classMap->offsets[i];
		auto cellValue = *reinterpret_cast<AnyCell*const*>(dataBase + nextCellOffset);

		h = combineHash(h, byteHasher(&dataBase[currentByte], nextCellOffset - currentByte));
		h = combineHash(h, (*this)(cellValue));

		currentByte = nextCellOffset + sizeof(AnyCell*);
	}

	return combineHash(h, byteHasher(&dataBase[currentByte], classMap->totalSize - currentByte));
}

}
