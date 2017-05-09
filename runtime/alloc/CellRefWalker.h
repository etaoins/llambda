#ifndef _LLIBY_ALLOC_CELLREFWALKER_H
#define _LLIBY_ALLOC_CELLREFWALKER_H

#include <cstdint>
#include <unordered_set>

#include "core/error.h"

#include "binding/UnitCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/IntegerCell.h"
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
#include "binding/MailboxCell.h"
#include "binding/HashMapCell.h"

#include "classmap/RecordClassMap.h"

#include "hash/DatumHashTree.h"

#include "dynamic/State.h"

namespace lliby
{

class DatumHashTree;

namespace dynamic
{
	class State;
}

namespace alloc
{


class CellRefWalker
{
public:
	/**
	 * Visits a cell by calling visitor
	 *
	 * @param state        Visitor state object. This is used to ensure cell references are visited exactly once.
	 * @param rootCellRef  Pointer to the root cell pointer to visit. The visitor may modify this pointer.
	 * @param visitor      Function to visit the cell. It is passed a pointer to the actual cell pointer to allow the cell
	 *                     to be relocated. If the function returns true the child cells of the passed cell will be visited
	 *                     next.
	 */
	template<typename T>
	void visitCell(AnyCell **rootCellRef, T visitor)
	{
	visitEntry:
		if (!visitor(rootCellRef))
		{
			// The visitor doesn't want to visit the child cells
			return;
		}
		else if (auto pairCell = cell_cast<PairCell>(*rootCellRef))
		{
			visitCell(pairCell->carRef(), visitor);

			// This is a bit of a hack
			// Even with templated function types tail recursion isn't guaranteed here. Clang 3.5 will in fact not convert
			// this to a tail call which causes excessive stack usage when visiting large proper lisss.
			rootCellRef = pairCell->cdrRef();
			goto visitEntry;
		}
		else if (auto vectorCell = cell_cast<VectorCell>(*rootCellRef))
		{
			for(VectorCell::LengthType i = 0; i < vectorCell->length(); i++)
			{
				// Use elements instead of elementsAt to skip the range check
				visitCell(&vectorCell->elements()[i], visitor);
			}
		}
		else if (auto recordLikeCell = cell_cast<RecordLikeCell>(*rootCellRef))
		{
			if (!recordLikeCell->isUndefined())
			{
				const RecordClassMap *classMap = recordLikeCell->classMap();

				for(std::uint32_t i = 0; i < classMap->offsetCount; i++)
				{
					const std::uint32_t byteOffset = classMap->offsets[i];
					auto cellRef = static_cast<std::uint8_t*>(recordLikeCell->dataBasePointer()) + byteOffset;

					visitCell(reinterpret_cast<AnyCell**>(cellRef), visitor);
				}
			}
		}
		else if (auto errorObjectCell = cell_cast<ErrorObjectCell>(*rootCellRef))
		{
			visitCell(reinterpret_cast<AnyCell**>(errorObjectCell->messageRef()), visitor);
			visitCell(reinterpret_cast<AnyCell**>(errorObjectCell->irritantsRef()), visitor);
		}
		else if (auto hashMapCell = cell_cast<HashMapCell>(*rootCellRef))
		{
			DatumHashTree::walkCellRefs(hashMapCell->datumHashTree(), *this, [&] (AnyCell **keyRef, AnyCell **valueRef)
			{
				visitCell(keyRef, visitor);
				visitCell(valueRef, visitor);
			});
		}
#ifndef NDEBUG
		else if (cell_cast<UnitCell>(*rootCellRef) ||
			cell_cast<EmptyListCell>(*rootCellRef) ||
			cell_cast<BooleanCell>(*rootCellRef) ||
			cell_cast<IntegerCell>(*rootCellRef) ||
			cell_cast<FlonumCell>(*rootCellRef) ||
			cell_cast<StringCell>(*rootCellRef) ||
			cell_cast<SymbolCell>(*rootCellRef) ||
			cell_cast<BytevectorCell>(*rootCellRef) ||
			cell_cast<CharCell>(*rootCellRef) ||
			cell_cast<PortCell>(*rootCellRef) ||
			cell_cast<MailboxCell>(*rootCellRef))
		{
			// No children
		}
		else
		{
			fatalError("Unknown cell type encountered attempting to visit children", *rootCellRef);
		}
#endif
	}

	/**
	 * Visits a CellRootList by calling a visitor
	 */
	template<typename T>
	void visitCellRootList(const CellRootList &cellRootList, T visitor)
	{
		for(auto node = cellRootList.head(); node != nullptr; node = node->next())
		{
			if (node->isInternal())
			{
				auto intNode = static_cast<InternalRootListNode*>(node);

				// Visit the embedded pointer
				if (intNode->cell() != nullptr)
				{
					visitCell(intNode->cellRef(), visitor);
				}
			}
			else
			{
				auto externNode = static_cast<ExternalRootListNode*>(node);

				// Visit each cell in this range
				for(std::size_t i = 0; i < externNode->cellCount(); i++)
				{
					auto cellRef = reinterpret_cast<AnyCell**>(&externNode->basePointer()[i]);

					if (*cellRef != nullptr)
					{
						visitCell(cellRef, visitor);
					}
				}
			}
		}
	}

	/**
	 * Visits a dynamic state
	 */
	template<typename T>
	void visitDynamicState(dynamic::State *state, T visitor)
	{
		dynamic::State::ParameterValueMap rebuiltMap;
		const std::size_t valueCount = state->selfValues().size();

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

		if (state->parent() != nullptr)
		{
			visitDynamicState(state->parent(), visitor);
		}
	}

	/**
	 * Determines if a datum hash subtree should be visited
	 *
	 * This is used to ensure datum hash leaf nodes are only visited once. If they were visited multiple times this
	 * would equivalent to multiple rooting a value which results in undefined behaviour.
	 */
	bool shouldVisitDatumHashSubtree(const DatumHashTree *subtree)
	{
		return m_visitedDatumHashSubtrees.insert(subtree).second;
	}

	std::unordered_set<const DatumHashTree*> m_visitedDatumHashSubtrees;
};

}
}

#endif

