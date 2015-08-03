#include "AnyCell.h"

#include <cstring>
#include <cmath>

#include "ExactIntegerCell.h"
#include "FlonumCell.h"
#include "SymbolCell.h"
#include "ProcedureCell.h"
#include "PairCell.h"
#include "VectorCell.h"
#include "BytevectorCell.h"
#include "StringCell.h"
#include "PortCell.h"
#include "DynamicStateCell.h"
#include "CharCell.h"
#include "MailboxCell.h"
#include "ErrorObjectCell.h"
#include "RecordCell.h"
#include "HashMapCell.h"

#include "hash/DatumHashTree.h"

#include "classmap/RecordClassMap.h"

namespace lliby
{

namespace
{

bool recordLikeIsEqual(const RecordLikeCell *recordLike1, const RecordLikeCell *recordLike2)
{
	if (recordLike1->recordClassId() != recordLike2->recordClassId())
	{
		return false;
	}

	const RecordClassMap *classMap = recordLike1->classMap();
	auto dataBase1 = static_cast<const std::uint8_t*>(recordLike1->dataBasePointer());
	auto dataBase2 = static_cast<const std::uint8_t*>(recordLike2->dataBasePointer());

	// Compare the non-cells for bitwise equality
	// XXX: This doesn't work quite correctly for NaN values but we don't have enough type information to distinguish
	// floating point fields. Most platforms only generate a single canonical NaN representation so this usually works
	// in practice.
	std::size_t currentByte = 0;
	for(std::uint32_t i = 0; i < classMap->offsetCount; i++)
	{
		std::size_t nextCellOffset = classMap->offsets[i];

		if (memcmp(&dataBase1[currentByte], &dataBase2[currentByte], nextCellOffset - currentByte))
		{
			return false;
		}

		auto cellValue1 = *reinterpret_cast<AnyCell*const*>(dataBase1 + nextCellOffset);
		auto cellValue2 = *reinterpret_cast<AnyCell*const*>(dataBase2 + nextCellOffset);

		if (!cellValue1->isEqual(cellValue2))
		{
			return false;
		}

		currentByte = nextCellOffset + sizeof(AnyCell*);
	}

	return !memcmp(&dataBase1[currentByte], &dataBase2[currentByte], classMap->totalSize - currentByte);
}

}

bool AnyCell::isEqv(const AnyCell *other) const
{
	if (this == other)
	{
		return true;
	}

	// These require more than address comparison by eqv?
	if (auto thisInteger = cell_cast<ExactIntegerCell>(this))
	{
		if (auto otherInteger = cell_cast<ExactIntegerCell>(other))
		{
			return thisInteger->value() == otherInteger->value();
		}
	}
	else if (auto thisFlonum = cell_cast<FlonumCell>(this))
	{
		if (auto otherFlonum = cell_cast<FlonumCell>(other))
		{
			const double thisValue = thisFlonum->value();
			const double otherValue = otherFlonum->value();

			if (std::isnan(thisValue) && std::isnan(otherValue))
			{
				// Both are NaN
				return true;
			}

			// Distinguish positive and negative zero
			if (std::signbit(thisValue) != std::signbit(otherValue))
			{
				return false;
			}

			return thisFlonum->value() == otherFlonum->value();
		}
	}
	else if (auto thisSymbol = cell_cast<SymbolCell>(this))
	{
		if (auto otherSymbol = cell_cast<SymbolCell>(other))
		{
			return *thisSymbol == *otherSymbol;
		}
	}
	else if (auto thisProcedure = cell_cast<ProcedureCell>(this))
	{
		if (auto otherProcedure = cell_cast<ProcedureCell>(other))
		{
			// If neither procedures captures variables and they have the same entry point then they are eqv?

			// This works around procedures being boxed in two different locations not being considered eqv?. R7RS allow us
			// to not recognize two procedures capturing the same variables as eqv?
			if ((!thisProcedure->capturesVariables() && !otherProcedure->capturesVariables()) &&
				 (thisProcedure->entryPoint() == otherProcedure->entryPoint()))
			{
				return true;
			}
		}
	}
	else if (auto thisString = cell_cast<StringCell>(this))
	{
		// R7RS doesn't require us to compare string contents so this isn't strictly required
		// However, we're already required to do this with symbols which have the exact same comparison logic. This also
		// makes eqv? on constant strings consistent between -O2 (which folds constants) and -O0 (which doesn't)
		if (auto otherString = cell_cast<StringCell>(other))
		{
			return *thisString == *otherString;
		}
	}
	else if (auto thisChar = cell_cast<CharCell>(this))
	{
		if (auto otherChar = cell_cast<CharCell>(other))
		{
			return thisChar->unicodeChar() == otherChar->unicodeChar();
		}
	}
	else if (auto thisPort = cell_cast<PortCell>(this))
	{
		if (auto otherPort = cell_cast<PortCell>(other))
		{
			return thisPort->port() == otherPort->port();
		}
	}

	return false;
}

bool AnyCell::isEqual(const AnyCell *other) const
{
	if (isEqv(other))
	{
		return true;
	}

	if (auto thisPair = cell_cast<PairCell>(this))
	{
		if (auto otherPair = cell_cast<PairCell>(other))
		{
			return thisPair->car()->isEqual(otherPair->car()) &&
				    thisPair->cdr()->isEqual(otherPair->cdr());
		}
	}
	else if (auto thisVector = cell_cast<VectorCell>(this))
	{
		if (auto otherVector = cell_cast<VectorCell>(other))
		{
			if (thisVector->length() != otherVector->length())
			{
				return false;
			}

			// Compare the vector element for element
			for(VectorCell::LengthType i = 0; i < thisVector->length(); i++)
			{
				if (!thisVector->elements()[i]->isEqual(otherVector->elements()[i]))
				{
					return false;
				}
			}

			return true;
		}
	}
	else if (auto thisBytevector = cell_cast<BytevectorCell>(this))
	{
		if (auto otherBytevector = cell_cast<BytevectorCell>(other))
		{
			if (thisBytevector->length() != otherBytevector->length())
			{
				return false;
			}

			// Compare the data byte for byte
			return memcmp(
					thisBytevector->byteArray()->data(),
					otherBytevector->byteArray()->data(),
					thisBytevector->length()
				) == 0;
		}
	}
	else if (auto thisMailboxCell = cell_cast<MailboxCell>(this))
	{
		if (auto otherMailboxCell = cell_cast<MailboxCell>(other))
		{
			std::shared_ptr<actor::Mailbox> thisMailbox = thisMailboxCell->lockedMailbox();
			std::shared_ptr<actor::Mailbox> otherMailbox = otherMailboxCell->lockedMailbox();

			// This considers closed mailboxes to be equal. This is similar to how Akka implicitly replaces all inboxes
			// of dead actors with the "dead-letter" inbox
			return thisMailbox == otherMailbox;
		}
	}
	else if (auto thisErrorObject = cell_cast<ErrorObjectCell>(this))
	{
		if (auto otherErrorObject = cell_cast<ErrorObjectCell>(other))
		{
			return (thisErrorObject->category() == otherErrorObject->category()) &&
				(thisErrorObject->message()->isEqual(otherErrorObject->message())) &&
				(thisErrorObject->irritants()->isEqual(otherErrorObject->irritants()));
		}
	}
	else if (auto thisRecord = cell_cast<RecordCell>(this))
	{
		if (auto otherRecord = cell_cast<RecordCell>(other))
		{
			return recordLikeIsEqual(thisRecord, otherRecord);
		}
	}
	else if (auto thisProcedure = cell_cast<ProcedureCell>(this))
	{
		if (auto otherProcedure = cell_cast<ProcedureCell>(other))
		{
			if (thisProcedure->entryPoint() != otherProcedure->entryPoint())
			{
				return false;
			}

			return recordLikeIsEqual(thisProcedure, otherProcedure);
		}
	}
	else if (auto thisHashMap = cell_cast<HashMapCell>(this))
	{
		if (auto otherHashMap = cell_cast<HashMapCell>(other))
		{
			auto thisHashTree = thisHashMap->datumHashTree();
			auto otherHashTree = otherHashMap->datumHashTree();

			if (DatumHashTree::size(thisHashTree) != DatumHashTree::size(otherHashTree))
			{
				return false;
			}

			return DatumHashTree::every(thisHashTree, [=] (AnyCell *key, AnyCell *thisValue)
			{
				AnyCell *otherValue = DatumHashTree::find(otherHashTree, key);
				return otherValue && thisValue->isEqual(otherValue);
			});
		}
	}

	return false;
}

void AnyCell::finalize()
{
	if (auto thisString = cell_cast<StringCell>(this))
	{
		thisString->finalizeString();
	}
	else if (auto thisSymbol = cell_cast<SymbolCell>(this))
	{
		thisSymbol->finalizeSymbol();
	}
	else if (auto thisVector = cell_cast<VectorCell>(this))
	{
		thisVector->finalizeVector();
	}
	else if (auto thisBytevector = cell_cast<BytevectorCell>(this))
	{
		thisBytevector->finalizeBytevector();
	}
	else if (auto thisRecordLike = cell_cast<RecordLikeCell>(this))
	{
		thisRecordLike->finalizeRecordLike();
	}
	else if (auto thisPort = cell_cast<PortCell>(this))
	{
		thisPort->finalizePort();
	}
	else if (auto thisDynamicState = cell_cast<DynamicStateCell>(this))
	{
		thisDynamicState->finalizeDynamicState();
	}
	else if (auto thisMailbox = cell_cast<MailboxCell>(this))
	{
		thisMailbox->finalizeMailbox();
	}
	else if (auto thisHashMap = cell_cast<HashMapCell>(this))
	{
		thisHashMap->finalizeHashMap();
	}
}

}
