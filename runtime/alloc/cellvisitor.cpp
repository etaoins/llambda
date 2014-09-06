#include "alloc/cellvisitor.h"

#include <iostream>
#include <cstdint>
#include <unordered_set>

#include "core/error.h"

#include "binding/UnitCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/BytevectorCell.h"
#include "binding/CharCell.h"
#include "binding/PairCell.h"
#include "binding/VectorCell.h"
#include "binding/RecordLikeCell.h"
#include "binding/ErrorObjectCell.h"
#include "binding/PortCell.h"
#include "binding/DynamicStateCell.h"

#include "classmap/RecordClassMap.h"

#include "dynamic/State.h"
#include "dynamic/Continuation.h"
#include "dynamic/EscapeProcedureCell.h"

#include "writer/ExternalFormDatumWriter.h"


namespace lliby
{
namespace alloc
{

void visitDynamicState(dynamic::State *state, std::function<bool(AnyCell **)> &visitor);

void visitCell(AnyCell **rootCellRef, std::function<bool(AnyCell **)> &visitor)
{
	if (!visitor(rootCellRef))
	{
		// The visitor doesn't want to visit the child cells
		return;
	}
	
	else if (auto pairCell = cell_cast<PairCell>(*rootCellRef))
	{
		visitCell(pairCell->carRef(), visitor);
		visitCell(pairCell->cdrRef(), visitor);
	}
	else if (auto vectorCell = cell_cast<VectorCell>(*rootCellRef))
	{
		for(std::uint32_t i = 0; i < vectorCell->length(); i++)
		{
			// Use elements instead of elementsAt to skip the range check
			visitCell(&vectorCell->elements()[i], visitor);
		}
	}
	else if (auto recordLikeCell = cell_cast<RecordLikeCell>(*rootCellRef))
	{
		const RecordClassOffsetMap *offsetMap = recordLikeCell->offsetMap();

		// Does this have any child cells and is it not undefined?
		if ((offsetMap != nullptr) && !recordLikeCell->isUndefined())
		{
			// Yes, iterate over them
			for(std::uint32_t i = 0; i < offsetMap->offsetCount; i++)
			{
				const std::uint32_t byteOffset = offsetMap->offsets[i]; 
				std::uint8_t *cellRef;

				if (recordLikeCell->dataIsInline())
				{
					// The data is stored inline inside the cell 
					cellRef = reinterpret_cast<std::uint8_t*>(recordLikeCell->recordDataRef()) + byteOffset;
				}
				else
				{
					cellRef = reinterpret_cast<std::uint8_t*>(recordLikeCell->recordData()) + byteOffset;
				}

				visitCell(reinterpret_cast<AnyCell**>(cellRef), visitor);
			}
		}
		else if (auto escapeProcCell = cell_cast<dynamic::EscapeProcedureCell>(*rootCellRef))
		{
			if (escapeProcCell->continuation() != nullptr)
			{
				visitContinuation(escapeProcCell->continuation(), visitor);
			}
		}
	}
	else if (auto errorObjectCell = cell_cast<ErrorObjectCell>(*rootCellRef))
	{
		visitCell(reinterpret_cast<AnyCell**>(errorObjectCell->messageRef()), visitor);
		visitCell(reinterpret_cast<AnyCell**>(errorObjectCell->irritantsRef()), visitor);
	}
	else if (auto dynamicStateCell = cell_cast<DynamicStateCell>(*rootCellRef))
	{
		if (dynamicStateCell->state() != nullptr)
		{
			visitDynamicState(dynamicStateCell->state(), visitor);
		}
	}
#ifndef NDEBUG
	else if (cell_cast<UnitCell>(*rootCellRef) ||
	    cell_cast<EmptyListCell>(*rootCellRef) ||
	    cell_cast<BooleanCell>(*rootCellRef) ||
	    cell_cast<ExactIntegerCell>(*rootCellRef) ||
	    cell_cast<FlonumCell>(*rootCellRef) ||
	    cell_cast<StringCell>(*rootCellRef) ||
	    cell_cast<SymbolCell>(*rootCellRef) ||
	    cell_cast<BytevectorCell>(*rootCellRef) ||
	    cell_cast<CharCell>(*rootCellRef) ||
	    cell_cast<PortCell>(*rootCellRef))
	{
		// No children
	}
	else
	{
		fatalError("Unknown cell type encountered attempting to visit children", *rootCellRef);
	}
#endif
}

void visitCellRefList(const CellRefRangeList &cellRefList, std::function<bool(AnyCell **)> &visitor)
{
	for(auto cellRefRange = cellRefList.head();
		cellRefRange != nullptr;
		cellRefRange = cellRefRange->next)
	{
		// Visit each cell in this range
		for(size_t i = 0; i < cellRefRange->cellCount; i++)
		{
			auto cellRef = reinterpret_cast<AnyCell**>(&cellRefRange->basePointer[i]);

			if (*cellRef != nullptr)
			{
				visitCell(cellRef, visitor);
			}
		}
	}
}

void visitShadowStack(ShadowStackEntry *head, std::function<bool(AnyCell **)> &visitor)
{
	for(ShadowStackEntry *stackEntry = head;
		 stackEntry != nullptr;
		 stackEntry = stackEntry->next)
	{
		for(std::uint64_t i = 0; i < stackEntry->cellCount; i++)
		{
			auto cellRef = &stackEntry->roots[i];

			if (*cellRef != nullptr)
			{
				visitCell(cellRef, visitor);
			}
		}
	}
}

void visitContinuation(dynamic::Continuation *continuation, std::function<bool(AnyCell **)> &visitor)
{
	visitCellRefList(continuation->strongRefs(), visitor);
	visitShadowStack(continuation->shadowStackHead(), visitor);
	visitCell(reinterpret_cast<AnyCell**>(continuation->dynamicStateCellRef()), visitor);

	if (*continuation->passedValuesRef())
	{
		visitCell(reinterpret_cast<AnyCell**>(continuation->passedValuesRef()), visitor);
	}

	// XXX: This isn't correct
	// This effectively treats all weak references inside the continuation's saved stack as a strong reference. This is
	// because the collector uses a special visitor for weak refs that we don't have access to. Even if it was passed
	// through all weak references need to be processed at the end of GC, not when we encounter the continuation.
	// As weak references are effectively unused this is close enough
	visitCellRefList(continuation->weakRefs(), visitor);
}

void visitDynamicState(dynamic::State *state, std::function<bool(AnyCell **)> &visitor)
{
	dynamic::State::ParameterValueMap rebuiltMap;
	const size_t valueCount = state->selfValues().size();

	// Visit the before and after procedures
	if (state->beforeProcedure())
	{
		visitCell(reinterpret_cast<AnyCell**>(state->beforeProcedureRef()), visitor);
	}
	
	if (state->afterProcedure())
	{
		visitCell(reinterpret_cast<AnyCell**>(state->afterProcedureRef()), visitor);
	}

	if (valueCount > 0)
	{
		// The new map will be the exact size of the old map
		rebuiltMap.reserve(valueCount);

		for(auto valueItem : state->selfValues())
		{
			dynamic::ParameterProcedureCell *paramProc = valueItem.first;
			AnyCell *value = valueItem.second;

			visitCell(reinterpret_cast<AnyCell**>(&paramProc), visitor);
			visitCell(reinterpret_cast<AnyCell**>(&value), visitor);

			rebuiltMap[paramProc] = value;
		}

		state->setSelfValues(rebuiltMap);
	}

	if (state->parentCell() != nullptr)
	{
		visitCell(reinterpret_cast<AnyCell**>(state->parentCellRef()), visitor);
	}
}

#ifndef _NDEBUG
void dumpReachableFrom(AnyCell *rootCell, bool dumpGlobalConstants)
{
	std::unordered_set<AnyCell*> shownCells;
	ExternalFormDatumWriter writer(std::cout);

	std::function<bool(AnyCell**)> visitor = [&] (AnyCell **cellRef) 
	{
		AnyCell *cell = *cellRef;

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
