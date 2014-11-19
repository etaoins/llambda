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

namespace lliby
{

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
			for(std::uint32_t i = 0; i < thisVector->length(); i++)
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
}

}
