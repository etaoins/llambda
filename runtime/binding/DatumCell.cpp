#include "DatumCell.h"

#include <string.h>

#include "ExactIntegerCell.h"
#include "InexactRationalCell.h"
#include "SymbolCell.h"
#include "ProcedureCell.h"
#include "PairCell.h"
#include "VectorCell.h"
#include "BytevectorCell.h"
#include "StringCell.h"

namespace lliby
{

bool DatumCell::isEqv(const DatumCell *other) const
{
	if (this == other)
	{
		return true;
	}

	// These require more than address comparison by eqv?
	if (auto thisInteger = datum_cast<ExactIntegerCell>(this))
	{
		if (auto otherInteger = datum_cast<ExactIntegerCell>(other))
		{
			return thisInteger->value() == otherInteger->value();
		}
	}
	else if (auto thisRational = datum_cast<InexactRationalCell>(this))
	{
		if (auto otherRational = datum_cast<InexactRationalCell>(other))
		{
			return thisRational->value() == otherRational->value();
		}
	}
	else if (auto thisSymbol = datum_cast<SymbolCell>(this))
	{
		if (auto otherSymbol = datum_cast<SymbolCell>(other))
		{
			return *thisSymbol == *otherSymbol;
		}
	}
	else if (auto thisProcedure = datum_cast<ProcedureCell>(this))
	{
		if (auto otherProcedure = datum_cast<ProcedureCell>(other))
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

	return false;
}

bool DatumCell::isEqual(const DatumCell *other) const
{
	if (isEqv(other))
	{
		return true;
	}

	if (auto thisPair = datum_cast<PairCell>(this))
	{
		if (auto otherPair = datum_cast<PairCell>(other))
		{
			return thisPair->car()->isEqual(otherPair->car()) &&
				    thisPair->cdr()->isEqual(otherPair->cdr());
		}
	}
	else if (auto thisVector = datum_cast<VectorCell>(this))
	{
		if (auto otherVector = datum_cast<VectorCell>(other))
		{
			if (thisVector->length() != otherVector->length())
			{
				return false;
			}

			// Compare the vector element for element
			for(std::uint32_t i = 0; i < thisVector->length(); i++)
			{
				if (!thisVector->elementAt(i)->isEqual(otherVector->elementAt(i)))
				{
					return false;
				}
			}

			return true;
		}
	}
	else if (auto thisBytevector = datum_cast<BytevectorCell>(this))
	{
		if (auto otherBytevector = datum_cast<BytevectorCell>(other))
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
	
void DatumCell::finalize()
{
	if (auto thisString = datum_cast<StringCell>(this))
	{
		thisString->finalizeString();
	}
	else if (auto thisSymbol = datum_cast<SymbolCell>(this))
	{
		thisSymbol->finalizeSymbol();
	}
	else if (auto thisVector = datum_cast<VectorCell>(this))
	{
		thisVector->finalizeVector();
	}
	else if (auto thisBytevector = datum_cast<BytevectorCell>(this))
	{
		thisBytevector->finalizeBytevector();
	}
	else if (auto thisRecordLike = datum_cast<RecordLikeCell>(this))
	{
		thisRecordLike->finalizeRecordLike();
	}
}

}
