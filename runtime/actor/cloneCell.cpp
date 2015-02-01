#include "actor/cloneCell.h"

#include <cstring>

#include "alloc/Heap.h"

#include "binding/ExactIntegerCell.h"
#include "binding/ProcedureCell.h"
#include "binding/RecordCell.h"
#include "binding/ExactIntegerCell.h"
#include "binding/FlonumCell.h"
#include "binding/BytevectorCell.h"
#include "binding/CharCell.h"
#include "binding/VectorCell.h"
#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/MailboxCell.h"

#include "dynamic/EscapeProcedureCell.h"

#include "classmap/RecordClassMap.h"

namespace lliby
{
namespace actor
{

namespace
{
	// Replaces a cell with a value that can be safely garbage collected
	void stubCell(void *placement)
	{
		new (placement) ExactIntegerCell(0);
	}

	AnyCell *cloneRecordLikeCell(alloc::Heap &heap, RecordLikeCell *recordLikeCell)
	{
		const RecordClassMap *classMap = recordLikeCell->classMap();
		const bool dataIsInline = recordLikeCell->dataIsInline();

		// Get a pointer to the old data
		void *oldData;

		if (dataIsInline)
		{
			// If our data is inline it starts at the record data pointer
			oldData = recordLikeCell->recordDataRef();
		}
		else
		{
			oldData = recordLikeCell->recordData();
		}

		// Create the new record data if we're out-of-line
		void *newData = nullptr;

		if (!dataIsInline)
		{
			newData = RecordLikeCell::allocateRecordData(classMap->totalSize);
		}

		// Allocate a new cell
		void *cellPlacement = heap.allocate();

		// Call the appropriate constructor for the cell type
		RecordLikeCell *newRecordLikeCell;

		if (auto procCell = cell_cast<ProcedureCell>(recordLikeCell))
		{
			newRecordLikeCell = new (cellPlacement) ProcedureCell(
					procCell->recordClassId(),
					dataIsInline,
					newData,
					procCell->entryPoint()
			);
		}
		else
		{
			// If we're not a ProcedureCell the only other subclass of RecordLikeCell is RecordCell
			auto recordCell = cell_unchecked_cast<RecordCell>(recordLikeCell);
			newRecordLikeCell = new (cellPlacement) RecordCell(recordCell->recordClassId(), dataIsInline, newData);
		}

		if (dataIsInline)
		{
			newData = recordLikeCell->recordDataRef();
		}

		std::memcpy(newData, oldData, classMap->totalSize);

		// Clone all the cell pointers
		for(std::uint32_t i = 0; i < classMap->offsetCount; i++)
		{
			const std::uint32_t byteOffset = classMap->offsets[i];

			auto cellRef = reinterpret_cast<AnyCell**>(reinterpret_cast<char*>(newData) + byteOffset);

			try
			{
				*cellRef = cloneCell(heap, *cellRef);
			}
			catch (UnclonableCellException &)
			{
				stubCell(cellPlacement);

				if (!dataIsInline)
				{
					RecordLikeCell::freeRecordData(newData);
				}

				throw;
			}
		}

		return newRecordLikeCell;
	}

	VectorCell *cloneVectorCell(alloc::Heap &heap, VectorCell *vectorCell)
	{
		AnyCell **newData = new AnyCell*[vectorCell->length()];

		try
		{
			for(VectorCell::LengthType i = 0; i < vectorCell->length(); i++)
			{
				newData[i] = cloneCell(heap, vectorCell->elements()[i]);
			}
		}
		catch (UnclonableCellException &)
		{
			delete[] newData;
			throw;
		}

		auto placement = heap.allocate();
		return new (placement) VectorCell(newData, vectorCell->length());
	}

	PairCell *clonePair(alloc::Heap &heap, PairCell *pairCell)
	{
		AnyCell *car = cloneCell(heap, pairCell->car());
		AnyCell *cdr = cloneCell(heap, pairCell->cdr());

		auto placement = heap.allocate();
		return new (placement) PairCell(car, cdr);
	}
}

AnyCell *cloneCell(alloc::Heap &heap, AnyCell *cell)
{
	if (cell->isGlobalConstant())
	{
		// This is a global constant; return it directly instead of copying as it accessible from all Worlds
		return cell;
	}

	if (auto exactIntCell = cell_cast<ExactIntegerCell>(cell))
	{
		auto placement = heap.allocate();
		return new (placement) ExactIntegerCell(exactIntCell->value());
	}
	else if (auto flonumCell = cell_cast<FlonumCell>(cell))
	{
		auto placement = heap.allocate();
		return new (placement) FlonumCell(flonumCell->value());
	}
	else if (auto charCell = cell_cast<CharCell>(cell))
	{
		auto placement = heap.allocate();
		return new (placement) CharCell(charCell->unicodeChar());
	}
	else if (auto bvCell = cell_cast<BytevectorCell>(cell))
	{
		auto placement = heap.allocate();
		return new (placement) BytevectorCell(bvCell->byteArray()->ref(), bvCell->length());
	}
	else if (auto vectorCell = cell_cast<VectorCell>(cell))
	{
		return cloneVectorCell(heap, vectorCell);
	}
	else if (auto stringCell = cell_cast<StringCell>(cell))
	{
		return stringCell->copy(heap);
	}
	else if (auto symbolCell = cell_cast<SymbolCell>(cell))
	{
		return symbolCell->copy(heap);
	}
	else if (auto pairCell = cell_cast<PairCell>(cell))
	{
		return clonePair(heap, pairCell);
	}
	else if (auto mailboxCell = cell_cast<MailboxCell>(cell))
	{
		auto placement = heap.allocate();
		return new (placement) MailboxCell(mailboxCell->mailbox());
	}
	else if (auto recordLikeCell = cell_cast<RecordLikeCell>(cell))
	{
		if (dynamic::EscapeProcedureCell::isInstance(cell))
		{
			throw UnclonableCellException(cell, "Escape procedures cannot be cloned");
		}
		else if (recordLikeCell->isUndefined())
		{
			throw UnclonableCellException(cell, "Undefined variable cannot be cloned");
		}
		else
		{
			return cloneRecordLikeCell(heap, recordLikeCell);
		}
	}

	throw UnclonableCellException(cell, "Not implemented");
}

}
}
