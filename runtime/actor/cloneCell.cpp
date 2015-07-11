#include "actor/cloneCell.h"

#include <cstring>
#include <sstream>
#include <unordered_map>

#include "alloc/Heap.h"
#include "core/error.h"

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
#include "binding/ErrorObjectCell.h"
#include "binding/PortCell.h"
#include "binding/ErrorCategory.h"

#include "dynamic/EscapeProcedureCell.h"
#include "dynamic/ParameterProcedureCell.h"
#include "dynamic/State.h"

#include "classmap/RecordClassMap.h"

namespace lliby
{
namespace actor
{

using dynamic::ParameterProcedureCell;
using dynamic::EscapeProcedureCell;
using dynamic::ConverterProcedureCell;
using dynamic::State;

namespace
{
	struct Context
	{
		// Dynamic state to resolve parameter procedures in
		State *captureState;
		std::unordered_map<AnyCell*, AnyCell*> clonedCells;
	};

	AnyCell *cachedClone(alloc::Heap &heap, AnyCell *cell, Context &context);

	// Replaces a cell with a value that can be safely garbage collected
	void stubCell(void *placement)
	{
		new (placement) ExactIntegerCell(0);
	}

	AnyCell *cloneParamProcCell(alloc::Heap &heap, ParameterProcedureCell *paramProcCell, Context &context)
	{
		using namespace dynamic;

		if (context.captureState == nullptr)
		{
			throw UnclonableCellException(paramProcCell, "Cannot clone parameter procedure with no dynamic state context");
		}

		// This is a bit tricky - give the cloned parameter procedure t
		AnyCell *initialValue = cachedClone(heap, context.captureState->valueForParameter(paramProcCell), context);

		ConverterProcedureCell *converterProc = nullptr;

		if (paramProcCell->converterProcedure() != nullptr)
		{
			converterProc = static_cast<ConverterProcedureCell*>(cachedClone(heap, paramProcCell->converterProcedure(), context));
		}

		auto placement = heap.allocate();
		return new (placement) ParameterProcedureCell(initialValue, converterProc);
	}

	AnyCell *cloneRecordLikeCell(alloc::Heap &heap, RecordLikeCell *recordLikeCell, Context &context)
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
			newData = newRecordLikeCell->recordDataRef();
		}

		std::memcpy(newData, oldData, classMap->totalSize);

		// Clone all the cell pointers
		for(std::uint32_t i = 0; i < classMap->offsetCount; i++)
		{
			const std::uint32_t byteOffset = classMap->offsets[i];

			auto cellRef = reinterpret_cast<AnyCell**>(reinterpret_cast<char*>(newData) + byteOffset);

			try
			{
				*cellRef = cachedClone(heap, *cellRef, context);
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

	VectorCell *cloneVectorCell(alloc::Heap &heap, VectorCell *vectorCell, Context &context)
	{
		AnyCell **newData = new AnyCell*[vectorCell->length()];

		try
		{
			for(VectorCell::LengthType i = 0; i < vectorCell->length(); i++)
			{
				newData[i] = cachedClone(heap, vectorCell->elements()[i], context);
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

	PairCell *clonePair(alloc::Heap &heap, PairCell *pairCell, Context &context)
	{
		AnyCell *car = cachedClone(heap, pairCell->car(), context);
		AnyCell *cdr = cachedClone(heap, pairCell->cdr(), context);

		auto placement = heap.allocate();
		return new (placement) PairCell(car, cdr);
	}

	ErrorObjectCell *cloneErrorObject(alloc::Heap &heap, ErrorObjectCell *errorObjectCell, Context &context)
	{
		StringCell *message = errorObjectCell->message()->copy(heap);
		auto irritants = static_cast<ProperList<AnyCell>*>(cachedClone(heap, errorObjectCell->irritants(), context));

		auto placement = heap.allocate();
		return new (placement) ErrorObjectCell(message, irritants, errorObjectCell->category());
	}

	AnyCell *uncachedClone(alloc::Heap &heap, AnyCell *cell, Context &context)
	{
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
			return cloneVectorCell(heap, vectorCell, context);
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
			return clonePair(heap, pairCell, context);
		}
		else if (auto mailboxCell = cell_cast<MailboxCell>(cell))
		{
			auto placement = heap.allocate();
			return new (placement) MailboxCell(mailboxCell->mailbox());
		}
		else if (auto errorObjectCell = cell_cast<ErrorObjectCell>(cell))
		{
			return cloneErrorObject(heap, errorObjectCell, context);
		}
		else if (auto recordLikeCell = cell_cast<RecordLikeCell>(cell))
		{
			if (auto paramProcCell = cell_cast<ParameterProcedureCell>(cell))
			{
				return cloneParamProcCell(heap, paramProcCell, context);
			}
			else if (EscapeProcedureCell::isInstance(cell))
			{
				throw UnclonableCellException(cell, "Escape procedures cannot be cloned");
			}
			else if (recordLikeCell->isUndefined())
			{
				throw UnclonableCellException(cell, "Undefined variables cannot be cloned");
			}
			else
			{
				return cloneRecordLikeCell(heap, recordLikeCell, context);
			}
		}
		else if (auto portCell = cell_cast<PortCell>(cell))
		{
			throw UnclonableCellException(portCell, "Ports cannot be cloned");
		}

		assert(false);
		throw UnclonableCellException(cell, "Unknown cell type");
	}

	AnyCell *cachedClone(alloc::Heap &heap, AnyCell *cell, Context &context)
	{
		if (cell->isGlobalConstant())
		{
			return cell;
		}

		auto cachedIt = context.clonedCells.find(cell);

		if (cachedIt != context.clonedCells.end())
		{
			// We've already cloned this cell; return the same one to keep (eqv?)
			return cachedIt->second;
		}

		// There isn't already a cloned cell; clone a new copy
		auto clonedCell = uncachedClone(heap, cell, context);
		context.clonedCells.emplace(cell, clonedCell);

		return clonedCell;
	}
}

AnyCell *cloneCell(alloc::Heap &heap, AnyCell *cell, State *captureState)
{
	Context context;
	context.captureState = captureState;

	if (cell->isGlobalConstant())
	{
		return cell;
	}

	// Don't bother searching for and caching the top-level cell
	return uncachedClone(heap, cell, context);
}

void UnclonableCellException::signalSchemeError(World &world, const char *procName)
{
	std::ostringstream msgStream;
	msgStream << "Unclonable value in " << procName << ": " << message();

	signalError(world, ErrorCategory::UnclonableValue, msgStream.str(), {cell()});
}

}
}
