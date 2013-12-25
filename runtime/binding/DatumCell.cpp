#include "DatumCell.h"
#include "ExactIntegerCell.h"
#include "InexactRationalCell.h"
#include "SymbolCell.h"
#include "ProcedureCell.h"

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
			// If neither procedures captures variables and they have the same entry
			// point then they are eqv?
			
			// This works around procedures being boxed in two different locations
			// not being considered eqv?. R7RS allow us to not recognize two
			// procedures capturing the same variables as eqv?:w
			if ((!thisProcedure->capturesVariables() && !otherProcedure->capturesVariables()) &&
				 (thisProcedure->entryPoint() == otherProcedure->entryPoint()))
			{
				return true;
			}
		}
	}

	return false;
}

}
