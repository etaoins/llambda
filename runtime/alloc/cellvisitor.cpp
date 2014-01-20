#include "alloc/cellvisitor.h"

#include <iostream>
#include <cstdint>
#include <set>

#include "core/fatal.h"

#include "binding/UnitCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/InexactRationalCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/BytevectorCell.h"
#include "binding/CharacterCell.h"
#include "binding/PairCell.h"
#include "binding/VectorCell.h"

#include "writer/ExternalFormDatumWriter.h"

namespace lliby
{
namespace alloc
{

void visitCell(DatumCell **rootCellRef, std::function<bool(DatumCell **)> &visitor)
{
	if (!visitor(rootCellRef))
	{
		// The visitor doesn't want to visit the child cells
		return;
	}
	
	if (datum_cast<UnitCell>(*rootCellRef) ||
	    datum_cast<EmptyListCell>(*rootCellRef) ||
	    datum_cast<BooleanCell>(*rootCellRef) ||
	    datum_cast<ExactIntegerCell>(*rootCellRef) ||
	    datum_cast<InexactRationalCell>(*rootCellRef) ||
	    datum_cast<StringCell>(*rootCellRef) ||
	    datum_cast<SymbolCell>(*rootCellRef) ||
	    datum_cast<BytevectorCell>(*rootCellRef) ||
	    datum_cast<CharacterCell>(*rootCellRef))
	{
		// No children
	}
	else if (auto pairCell = datum_cast<PairCell>(*rootCellRef))
	{
		visitCell(pairCell->carRef(), visitor);
		visitCell(pairCell->cdrRef(), visitor);
	}
	else if (auto vectorCell = datum_cast<VectorCell>(*rootCellRef))
	{
		for(std::uint32_t i = 0; i < vectorCell->length(); i++)
		{
			// Use elements instead of elementsAt to skip the range check
			visitCell(&vectorCell->elements()[i], visitor);
		}
	}
	else
	{
		_lliby_fatal("Unknown cell type encountered attempting to visit children", *rootCellRef);
	}
}

void dumpReachableFrom(DatumCell *rootCell, bool dumpGlobalConstants)
{
	std::set<DatumCell*> showCells;
	ExternalFormDatumWriter writer(std::cout);

	std::function<bool(DatumCell**)> visitor = [&] (DatumCell **cellRef) 
	{
		DatumCell *cell = *cellRef;

		if (!dumpGlobalConstants && (cell->gcState() == GarbageState::GlobalConstant))
		{
			return false;
		}

		if (showCells.count(cell) > 0)
		{
			return false;
		}

		std::cout << reinterpret_cast<void*>(cell) << ": ";
		writer.render(cell);
		std::cout << std::endl;

		showCells.insert(cell);
		return true;
	};

	visitCell(&rootCell, visitor);
}

}
}
