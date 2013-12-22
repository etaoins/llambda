#include "DatumCell.h"
#include "ExactIntegerCell.h"
#include "InexactRationalCell.h"
#include "SymbolCell.h"

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

	return false;
}

}
