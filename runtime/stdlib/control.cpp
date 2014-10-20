#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/ListElementCell.h"
#include "binding/EmptyListCell.h"
#include "binding/ProperList.h"
#include "binding/VectorCell.h"
#include "binding/UnitCell.h"
#include "binding/RestArgument.h"
#include "binding/CharCell.h"
#include "binding/StringCell.h"
#include "unicode/UnicodeChar.h"

#include "alloc/allocator.h"
#include "alloc/WeakRef.h"
#include "alloc/RangeAlloc.h"
#include "alloc/cellref.h"

#include "dynamic/EscapeProcedureCell.h"
#include "dynamic/Continuation.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	using AnyMapProcedureCell = TypedProcedureCell<AnyCell *, AnyCell *, ListElementCell *>;
	using StringMapProcedureCell = TypedProcedureCell<UnicodeChar, UnicodeChar, ListElementCell *>;

	/**
	 * Variant of cell_cast that raises an error if the cast fails
	 *
	 * This is used during (map) to defend against the input lists being modified during the (map) operation causing
	 * crashes.
	 */
	template<class T>
	T *cell_map_cast(World &world, AnyCell *value)
	{
		T *castResult = cell_cast<T>(value);

		if (castResult == nullptr)
		{
			signalError(world, "Input list mutated during (map)");
		}

		return castResult;
	}
}

extern "C"
{

ReturnValuesList *lliby_apply(World &world, TopProcedureCell *procedure, RestArgument<AnyCell> *argHead)
{
	ListElementCell *procArgHead;

	// Do everything inside the block so the ProperList/StrongRefRange is destructed before calling the procedure
	// This reduces the resources we use during recursive calls to (apply) and might allow the compiler to perform
	// tail call optimization
	{
		// Find our arguments
		ProperList<AnyCell> applyArgList(argHead);

		if (applyArgList.length() == 0)
		{
			// This is easy - call with no args
			procArgHead = argHead;
		}
		else
		{
			// Build our procedure args
			auto applyArgIt = applyArgList.begin();
			
			// Standalone args are zero or more args that appear before the final proper list
			auto standaloneArgCount = applyArgList.length() - 1;
			std::vector<AnyCell*> standaloneArgs(standaloneArgCount);

			for(ProperList<AnyCell>::size_type i = 0; i < standaloneArgCount; i++)
			{
				standaloneArgs[i] = *(applyArgIt++);
			}

			// Ensure the final argument is a proper list
			// This would violate our calling convention otherwise
			auto finalListHead = cell_cast<ListElementCell>(*applyArgIt);

			if (!(finalListHead && ProperList<AnyCell>(finalListHead).isValid()))
			{
				signalError(world, "Final argument to (apply) must be a proper list", {*applyArgIt});
			}

			// Reference the procedure cell before allocating the argument list
			alloc::StrongRefRange<TopProcedureCell> procedureRef(world, &procedure, 1);	

			// We verified the final arg is a proper list so this must also be a proper list
			procArgHead = cell_unchecked_cast<ListElementCell>(ListElementCell::createList(world, standaloneArgs, finalListHead));
		}
	}

	return procedure->apply(world, procArgHead);
}

ReturnValuesList *lliby_values(RestArgument<AnyCell> *restArgHead)
{
	return restArgHead;
}

ReturnValuesList *lliby_call_with_current_continuation(World &world, TypedProcedureCell<ReturnValuesList*, ProcedureCell*> *proc)
{
	using dynamic::Continuation;
	using dynamic::EscapeProcedureCell;

	// This is the procedure we're calling
	alloc::StrongRef<TypedProcedureCell<ReturnValuesList*, ProcedureCell*>> procRef(world, proc);
		
	// Create the escape procedure and its args
	// We build this first as there's no way to GC root a continuation at the moment. This also make sure the 
	// continuation stays rooted in the resume path so we can access cont->passedValue(). Otherwise switching dynamic
	// states back to the original continuation could free the the continuation.
	alloc::StrongRef<EscapeProcedureCell> escapeRef(world, EscapeProcedureCell::createInstance(world, nullptr));

	// Capture the current continuation
	Continuation *cont = Continuation::capture(world);
	
	if (ListElementCell *passedValues = cont->takePassedValues())
	{
		// We're the result of a continuation being invoked
		return passedValues;
	}
	else
	{
		// We're the original code flow path 
		// Set the continuation on the escape proc - this will make the continuation reachable from GC
		escapeRef->setContinuation(cont);
	
		// Invoke the procedure passing in the escape proc
		// If it returns without invoking the escape proc we'll return through here
		return procRef->apply(world, escapeRef);
	}
}

ReturnValuesList *lliby_call_with_values(World &world, ThunkProcedureCell *producer, TopProcedureCell *consumerRaw)
{
	alloc::StrongRef<TopProcedureCell> consumer(world, consumerRaw);

	ReturnValuesList *values = producer->apply(world);
	return consumer->apply(world, values);
}

VectorCell *lliby_vector_map(World &world, AnyMapProcedureCell *mapProcRaw, VectorCell *firstVectorRaw, RestArgument<VectorCell> *argHead)
{
	ProperList<VectorCell> restVectorList(argHead);
	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	// This is the minimum length of all of our input vectos
	std::uint32_t minimumLength = firstVectorRaw->length();

	// Build our vector of input vector cells
	std::vector<VectorCell*> restVectors;
	restVectors.reserve(restVectorList.length());

	for(auto restVector : restVectorList)
	{
		restVectors.push_back(restVector);
		minimumLength = std::min(minimumLength, restVector->length());
	}

	// Root the input vector
	alloc::StrongRef<VectorCell> firstVector(world, firstVectorRaw);

	// Root the input vectors before we allocate the output vector and input lists
	alloc::StrongRefRange<VectorCell> restVectorsRoot(world, restVectors);

	// Allocate the output vector
	VectorCell *outputVectorRaw = VectorCell::fromFill(world, minimumLength, UnitCell::instance());
	alloc::StrongRef<VectorCell> outputVector(world, outputVectorRaw);

	for(std::uint32_t i = 0; i < minimumLength; i++)
	{
		// Build the rest argument list
		std::vector<AnyCell*> restArgVector;
		restArgVector.reserve(restVectors.size());

		for(auto restVector : restVectors)
		{
			// Use elements() directly because we already checked the length of all the vectors
			// This lets us skip the bounds check
			restArgVector.push_back(restVector->elements()[i]);
		}

		ListElementCell *restArgList = ListElementCell::createProperList(world, restArgVector);

		AnyCell *result = mapProc->apply(world, firstVector->elements()[i], restArgList);
		outputVector->elements()[i] = result;
	}

	return outputVector;
}

ListElementCell *lliby_map(World &world, AnyMapProcedureCell *mapProcRaw, ListElementCell *firstListRaw, RestArgument<ListElementCell>* restListsRaw)
{
	alloc::StrongRef<ListElementCell> firstList(world, firstListRaw);
	std::vector<alloc::StrongRef<ListElementCell>> restLists;

	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	// This is the minimum length of all of our input lists first
	std::uint32_t minimumLength = ProperList<AnyCell>(firstList).length();

	for(auto restListHead : ProperList<ListElementCell>(restListsRaw))
	{
		// Create the strong ref for the rest list
		restLists.emplace(restLists.end(), world, restListHead);

		std::uint32_t restListLength = ProperList<AnyCell>(restListHead).length();
		minimumLength = std::min(minimumLength, restListLength);
	}

	// Create the vector of output values and GC root it
	std::vector<AnyCell*> outputVector(minimumLength, nullptr);

	// Place the outputVectorRoot inside a block to ensure we don't double GC root the same memory
	{
		alloc::StrongRefRange<AnyCell> outputVectorRoot(world, outputVector);

		for(std::uint32_t i = 0; i < minimumLength; i++)
		{
			// Build the rest argument list
			std::vector<AnyCell*> restArgVector;
			restArgVector.reserve(restLists.size());

			for(auto restList : restLists)
			{
				auto restListPair = cell_map_cast<PairCell>(world, restList.data());
				restArgVector.push_back(restListPair->car());

				// Move this forward to the next element
				restList.setData(cell_map_cast<ListElementCell>(world, restListPair->cdr()));
			}

			// Create the rest argument list
			ListElementCell *restArgList = ListElementCell::createProperList(world, restArgVector);

			// Extract the first list value and move it forward
			auto firstListPair = cell_map_cast<PairCell>(world, firstList.data());
			firstList.setData(cell_map_cast<ListElementCell>(world, firstListPair->cdr()));

			AnyCell *result = mapProc->apply(world, firstListPair->car(), restArgList);
			outputVector[i] = result;
		}
	}

	return ListElementCell::createProperList(world, outputVector);
}

StringCell *lliby_string_map(World &world, StringMapProcedureCell *mapProcRaw, StringCell *firstString, RestArgument<StringCell> *restStrings)
{
	// Extract the code points from the string argument. Once this is done we no longer need the original strings
	std::vector<UnicodeChar> firstCharVector(firstString->unicodeChars());

	ProperList<StringCell> restStringList(restStrings);
	std::vector<std::vector<UnicodeChar>> restCharVectors;

	std::size_t minimumLength = firstCharVector.size();

	for(auto restString : restStringList)
	{
		auto newVectorIt = restCharVectors.emplace(restCharVectors.end(), restString->unicodeChars());
		minimumLength = std::min(minimumLength, newVectorIt->size());
	}

	std::vector<UnicodeChar> outputVector;
	outputVector.resize(minimumLength);

	alloc::StrongRef<StringMapProcedureCell> mapProc(world, mapProcRaw);

	for(std::size_t i = 0; i < minimumLength; i++)
	{
		// Build the rest argument list
		std::vector<AnyCell*> restArgVector;
		restArgVector.reserve(restCharVectors.size());

		auto boxedCharAllocIt = alloc::allocateCells(world, restCharVectors.size());
		for(auto restCharVector : restCharVectors)
		{
			restArgVector.push_back(new (boxedCharAllocIt++) CharCell(restCharVector[i]));
		}

		// Create the rest argument list
		ListElementCell *restArgList = ListElementCell::createProperList(world, restArgVector);

		outputVector[i] = mapProc->apply(world, firstCharVector[i], restArgList);
	}

	return StringCell::fromUnicodeChars(world, outputVector);
}

}
