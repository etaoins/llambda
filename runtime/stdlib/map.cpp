#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/VectorCell.h"
#include "binding/CharCell.h"
#include "binding/StringCell.h"
#include "unicode/UnicodeChar.h"
#include "binding/RestArgument.h"
#include "binding/ProperList.h"
#include "binding/UnitCell.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"
#include "alloc/cellref.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	using AnyMapProcedureCell = TypedProcedureCell<AnyCell *, AnyCell *, ListElementCell *>;
	using AnyIteratorProcedureCell = TypedProcedureCell<void, AnyCell *, ListElementCell *>;

	using StringMapProcedureCell = TypedProcedureCell<UnicodeChar, UnicodeChar, ListElementCell *>;
	using StringIteratorProcedureCell = TypedProcedureCell<UnicodeChar, UnicodeChar, ListElementCell *>;

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

	template<typename MapFunction>
	VectorCell* abstractVectorMap(World &world, MapFunction mapFunc, VectorCell *firstVectorRaw, RestArgument<VectorCell> *argHead)
	{
		ProperList<VectorCell> restVectorList(argHead);

		// This is the minimum length of all of our input vectors
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

		// Create the output vector and GC root it
		alloc::VectorRef outputVector(world, VectorCell::fromFill(world, minimumLength, UnitCell::instance()));

		for(std::uint32_t i = 0; i < minimumLength; i++)
		{
			// Build the rest argument list
			std::vector<AnyCell*> restArgVector;
			restArgVector.reserve(restVectors.size());

			for(auto restVector : restVectors)
			{
				restArgVector.push_back(restVector->elements()[i]);
			}

			ListElementCell *restArgList = ListElementCell::createProperList(world, restArgVector);
			AnyCell *result = mapFunc(firstVector->elements()[i], restArgList);

			// Use elements() here to skip the bounds check that setElementAt() will perform
			outputVector->elements()[i] = result;
		}

		return outputVector;
	}

	template<typename MapFunction>
	std::vector<AnyCell*> abstractListMap(World &world, MapFunction mapFunc, ListElementCell *firstListRaw, RestArgument<ListElementCell>* restListsRaw)
	{
		alloc::StrongRef<ListElementCell> firstList(world, firstListRaw);
		std::vector<alloc::StrongRef<ListElementCell>> restLists;

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

			outputVector[i] = mapFunc(firstListPair->car(), restArgList);
		}

		return outputVector;
	}

	template<typename MapFunction>
	std::vector<UnicodeChar> abstractStringMap(World &world, MapFunction mapFunc, StringCell *firstString, RestArgument<StringCell> *restStrings)
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

			outputVector[i] = mapFunc(firstCharVector[i], restArgList);
		}

		return outputVector;
	}
}

extern "C"
{

VectorCell *lliby_vector_map(World &world, AnyMapProcedureCell *mapProcRaw, VectorCell *firstVectorRaw, RestArgument<VectorCell> *argHead)
{
	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, ListElementCell *restArgs) {
		return mapProc->apply(world, firstArg, restArgs);
	};

	return abstractVectorMap(world, mapFunc, firstVectorRaw, argHead);
}

void lliby_vector_for_each(World &world, AnyIteratorProcedureCell *mapProcRaw, VectorCell *firstVectorRaw, RestArgument<VectorCell> *argHead)
{
	alloc::StrongRef<AnyIteratorProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, ListElementCell *restArgs) {
		mapProc->apply(world, firstArg, restArgs);
		return UnitCell::instance();
	};

	abstractVectorMap(world, mapFunc, firstVectorRaw, argHead);
}

ListElementCell *lliby_map(World &world, AnyMapProcedureCell *mapProcRaw, ListElementCell *firstListRaw, RestArgument<ListElementCell>* argHead)
{
	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, ListElementCell *restArgs) {
		return mapProc->apply(world, firstArg, restArgs);
	};

	std::vector<AnyCell*> result = abstractListMap(world, mapFunc, firstListRaw, argHead);
	return ListElementCell::createProperList(world, result);
}

void lliby_for_each(World &world, AnyIteratorProcedureCell *mapProcRaw, ListElementCell *firstListRaw, RestArgument<ListElementCell>* argHead)
{
	alloc::StrongRef<AnyIteratorProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, ListElementCell *restArgs) {
		mapProc->apply(world, firstArg, restArgs);
		return UnitCell::instance();
	};

	abstractListMap(world, mapFunc, firstListRaw, argHead);
}

StringCell *lliby_string_map(World &world, StringMapProcedureCell *mapProcRaw, StringCell *firstString, RestArgument<StringCell> *argHead)
{
	alloc::StrongRef<StringMapProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (UnicodeChar firstArg, ListElementCell *restArgs) {
		return mapProc->apply(world, firstArg, restArgs);
	};

	std::vector<UnicodeChar> result = abstractStringMap(world, mapFunc, firstString, argHead);
	return StringCell::fromUnicodeChars(world, result);
}

void lliby_string_for_each(World &world, StringIteratorProcedureCell *mapProcRaw, StringCell *firstString, RestArgument<StringCell> *argHead)
{
	alloc::StrongRef<StringIteratorProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (UnicodeChar firstArg, ListElementCell *restArgs) {
		mapProc->apply(world, firstArg, restArgs);
		return UnicodeChar(0);
	};

	abstractStringMap(world, mapFunc, firstString, argHead);
}

}
