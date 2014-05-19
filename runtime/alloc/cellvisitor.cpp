#include "alloc/cellvisitor.h"

#include <iostream>
#include <cstdint>
#include <unordered_set>

#include "core/error.h"

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
#include "binding/RecordLikeCell.h"
#include "binding/ErrorObjectCell.h"
#include "binding/PortCell.h"

#include "classmap/RecordClassMap.h"

#include "dynamic/State.h"

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
	    datum_cast<CharacterCell>(*rootCellRef) ||
	    datum_cast<PortCell>(*rootCellRef))
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
	else if (auto recordLikeCell = datum_cast<RecordLikeCell>(*rootCellRef))
	{
		const RecordClassOffsetMap *offsetMap = recordLikeCell->offsetMap();

		// Does this have any child cells
		if (offsetMap != nullptr)
		{
			// Yes, iterator over them
			for(std::uint32_t i = 0; i < offsetMap->offsetCount; i++)
			{
				const std::uint32_t byteOffset = offsetMap->offsets[i]; 
				std::uint8_t *datumRefLocation;

				if (recordLikeCell->dataIsInline())
				{
					// The data is stored inline inside the cell 
					datumRefLocation = reinterpret_cast<std::uint8_t*>(recordLikeCell->recordDataRef()) + byteOffset;
				}
				else
				{
					datumRefLocation = reinterpret_cast<std::uint8_t*>(recordLikeCell->recordData()) + byteOffset;
				}

				auto datumRef = reinterpret_cast<DatumCell**>(datumRefLocation);

				// codegen can create null pointers as placeholder for undefined values
				// This is used to implement recursive values
				if (*datumRef != nullptr)
				{
					visitCell(datumRef, visitor);
				}
			}
		}
	}
	else if (auto errorObjectCell = datum_cast<ErrorObjectCell>(*rootCellRef))
	{
		visitCell(reinterpret_cast<DatumCell**>(errorObjectCell->messageRef()), visitor);
		visitCell(reinterpret_cast<DatumCell**>(errorObjectCell->irritantsRef()), visitor);
	}
	else
	{
		fatalError("Unknown cell type encountered attempting to visit children", *rootCellRef);
	}
}

void visitDynamicState(dynamic::State *state, std::function<bool(DatumCell **)> &visitor)
{
	dynamic::State::ParameterValueMap rebuiltMap;
	const size_t valueCount = state->selfValues().size();

	// Visit the before and after procedures
	if (state->beforeProcedure())
	{
		visitCell(reinterpret_cast<DatumCell**>(state->beforeProcedureRef()), visitor);
	}
	
	if (state->afterProcedure())
	{
		visitCell(reinterpret_cast<DatumCell**>(state->afterProcedureRef()), visitor);
	}

	if (valueCount > 0)
	{
		// The new map will be the exact size of the old map
		rebuiltMap.reserve(valueCount);

		for(auto valueItem : state->selfValues())
		{
			dynamic::ParameterProcedureCell *paramProc = valueItem.first;
			DatumCell *value = valueItem.second;

			visitCell(reinterpret_cast<DatumCell**>(&paramProc), visitor);
			visitCell(reinterpret_cast<DatumCell**>(&value), visitor);

			rebuiltMap[paramProc] = value;
		}

		state->setSelfValues(rebuiltMap);
	}

	if (state->parent() != nullptr)
	{
		visitDynamicState(state->parent(), visitor);
	}
}

#ifndef _NDEBUG
void dumpReachableFrom(DatumCell *rootCell, bool dumpGlobalConstants)
{
	std::unordered_set<DatumCell*> shownCells;
	ExternalFormDatumWriter writer(std::cout);

	std::function<bool(DatumCell**)> visitor = [&] (DatumCell **cellRef) 
	{
		DatumCell *cell = *cellRef;

		if (!dumpGlobalConstants && (cell->gcState() == GarbageState::GlobalConstant))
		{
			return false;
		}

		if (shownCells.count(cell) > 0)
		{
			return false;
		}

		std::cout << reinterpret_cast<void*>(cell) << ": ";
		writer.render(cell);
		std::cout << std::endl;

		shownCells.insert(cell);
		return true;
	};

	visitCell(&rootCell, visitor);
}
#endif

}
}
