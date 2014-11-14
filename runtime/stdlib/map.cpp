#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/VectorCell.h"
#include "binding/CharCell.h"
#include "binding/StringCell.h"
#include "unicode/UnicodeChar.h"
#include "binding/ProperList.h"
#include "binding/UnitCell.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"
#include "alloc/StrongRefVector.h"
#include "alloc/cellref.h"

#include "util/StringCellBuilder.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	using AnyMapProcedureCell = TypedProcedureCell<AnyCell *, AnyCell *, ProperList<AnyCell> *>;
	using AnyIteratorProcedureCell = TypedProcedureCell<void, AnyCell *, ProperList<AnyCell> *>;

	using StringMapProcedureCell = TypedProcedureCell<UnicodeChar::CodePoint, UnicodeChar, ProperList<CharCell> *>;
	using StringIteratorProcedureCell = TypedProcedureCell<UnicodeChar::CodePoint, UnicodeChar, ProperList<CharCell> *>;

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
	VectorCell* abstractVectorMap(World &world, MapFunction mapFunc, VectorCell *firstVectorRaw, ProperList<VectorCell> *restVectorList)
	{
		// This is the minimum length of all of our input vectors
		std::uint32_t minimumLength = firstVectorRaw->length();

		// Build our vector of input vector cells
		alloc::StrongRefVector<VectorCell> restVectors(world, restVectorList->begin(), restVectorList->end());

		// Root the input vector
		alloc::StrongRef<VectorCell> firstVector(world, firstVectorRaw);

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

			ProperList<AnyCell> *restArgList = ProperList<AnyCell>::create(world, restArgVector);
			AnyCell *result = mapFunc(firstVector->elements()[i], restArgList);

			// Use elements() here to skip the bounds check that setElementAt() will perform
			outputVector->elements()[i] = result;
		}

		return outputVector;
	}

	template<typename MapFunction>
	alloc::StrongRefVector<AnyCell> abstractListMap(World &world, MapFunction mapFunc, ProperList<AnyCell> *firstListRaw, ProperList<ProperList<AnyCell>> *restListsRaw)
	{
		alloc::StrongRef<ListElementCell> firstList(world, firstListRaw);
		alloc::StrongRefVector<ListElementCell> restLists(world);

		// This is the minimum length of all of our input lists first
		std::uint32_t minimumLength = firstListRaw->size();

		for(auto restList : *restListsRaw)
		{
			// Create the strong ref for the rest list
			restLists.push_back(restList);

			minimumLength = std::min(minimumLength, restList->size());
		}

		// Create the vector of output values
		alloc::StrongRefVector<AnyCell> outputVector(world, minimumLength, nullptr);

		for(std::uint32_t i = 0; i < minimumLength; i++)
		{
			// Build the rest argument list
			std::vector<AnyCell*> restArgVector;
			restArgVector.reserve(restLists.size());

			for(ListElementCell* &restList : restLists)
			{
				auto restListPair = cell_map_cast<PairCell>(world, restList);
				restArgVector.push_back(restListPair->car());

				// Move this forward to the next element
				restList = cell_map_cast<ListElementCell>(world, restListPair->cdr());
			}

			// Create the rest argument list
			ProperList<AnyCell> *restArgList = ProperList<AnyCell>::create(world, restArgVector);

			// Extract the first list value and move it forward
			auto firstListPair = cell_map_cast<PairCell>(world, firstList.data());
			firstList.setData(cell_map_cast<ListElementCell>(world, firstListPair->cdr()));

			outputVector[i] = mapFunc(firstListPair->car(), restArgList);
		}

		return outputVector;
	}

	template<typename MapFunction>
	StringCellBuilder abstractStringMap(World &world, MapFunction mapFunc, StringCell *firstString, ProperList<StringCell> *restStringList)
	{
		// Extract the code points from the string argument. Once this is done we no longer need the original strings
		std::vector<UnicodeChar> firstCharVector(firstString->unicodeChars());

		std::size_t minimumLength = firstCharVector.size();

		std::vector<std::vector<UnicodeChar>> restCharVectors;
		for(auto restString : *restStringList)
		{
			auto newVectorIt = restCharVectors.emplace(restCharVectors.end(), restString->unicodeChars());
			minimumLength = std::min(minimumLength, newVectorIt->size());
		}

		StringCellBuilder builder(minimumLength);

		for(std::size_t i = 0; i < minimumLength; i++)
		{
			// Build the rest argument list
			std::vector<CharCell*> restArgVector;
			restArgVector.reserve(restCharVectors.size());

			auto boxedCharAllocIt = alloc::allocateCells(world, restCharVectors.size());
			for(auto restCharVector : restCharVectors)
			{
				restArgVector.push_back(new (boxedCharAllocIt++) CharCell(restCharVector[i]));
			}

			// Create the rest argument list
			ProperList<CharCell> *restArgList = ProperList<CharCell>::create(world, restArgVector);

			UnicodeChar result(mapFunc(firstCharVector[i], restArgList));
			if (!result.isValid())
			{
				signalError(world, "(string-map) mapping procedure returned invalid character");
			}
			builder << result;
		}

		return builder;
	}
}

extern "C"
{

VectorCell *lliby_vector_map(World &world, AnyMapProcedureCell *mapProcRaw, VectorCell *firstVectorRaw, ProperList<VectorCell> *argHead)
{
	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, ProperList<AnyCell> *restArgs) {
		return mapProc->apply(world, firstArg, restArgs);
	};

	return abstractVectorMap(world, mapFunc, firstVectorRaw, argHead);
}

void lliby_vector_for_each(World &world, AnyIteratorProcedureCell *mapProcRaw, VectorCell *firstVectorRaw, ProperList<VectorCell> *argHead)
{
	alloc::StrongRef<AnyIteratorProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, ProperList<AnyCell> *restArgs) {
		mapProc->apply(world, firstArg, restArgs);
		return UnitCell::instance();
	};

	abstractVectorMap(world, mapFunc, firstVectorRaw, argHead);
}

ProperList<AnyCell> *lliby_map(World &world, AnyMapProcedureCell *mapProcRaw, ProperList<AnyCell> *firstListRaw, ProperList<ProperList<AnyCell>>* argHead)
{
	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, ProperList<AnyCell> *restArgs) {
		return mapProc->apply(world, firstArg, restArgs);
	};

	alloc::StrongRefVector<AnyCell> result = abstractListMap(world, mapFunc, firstListRaw, argHead);
	return ProperList<AnyCell>::create(world, result);
}

void lliby_for_each(World &world, AnyIteratorProcedureCell *mapProcRaw, ProperList<AnyCell> *firstListRaw, ProperList<ProperList<AnyCell>>* argHead)
{
	alloc::StrongRef<AnyIteratorProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, ProperList<AnyCell> *restArgs) {
		mapProc->apply(world, firstArg, restArgs);
		return UnitCell::instance();
	};

	abstractListMap(world, mapFunc, firstListRaw, argHead);
}

StringCell *lliby_string_map(World &world, StringMapProcedureCell *mapProcRaw, StringCell *firstString, ProperList<StringCell> *argHead)
{
	alloc::StrongRef<StringMapProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (UnicodeChar firstArg, ProperList<CharCell> *restArgs) {
		return mapProc->apply(world, firstArg, restArgs);
	};

	StringCellBuilder builder(abstractStringMap(world, mapFunc, firstString, argHead));
	return builder.result(world);
}

void lliby_string_for_each(World &world, StringIteratorProcedureCell *mapProcRaw, StringCell *firstString, ProperList<StringCell> *argHead)
{
	alloc::StrongRef<StringIteratorProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (UnicodeChar firstArg, ProperList<CharCell> *restArgs) {
		mapProc->apply(world, firstArg, restArgs);
		return UnicodeChar(0);
	};

	abstractStringMap(world, mapFunc, firstString, argHead);
}

}
