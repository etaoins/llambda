#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/VectorCell.h"
#include "binding/CharCell.h"
#include "binding/StringCell.h"
#include "unicode/UnicodeChar.h"
#include "binding/ProperList.h"
#include "binding/UnitCell.h"

#include "alloc/StrongRefVector.h"
#include "alloc/cellref.h"

#include "util/StringCellBuilder.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	using AnyMapProcedureCell = TypedProcedureCell<AnyCell*, AnyCell*, RestValues<AnyCell>*>;
	using AnyIteratorProcedureCell = TypedProcedureCell<void, AnyCell*, RestValues<AnyCell>*>;

	using StringMapProcedureCell = TypedProcedureCell<UnicodeChar::CodePoint, UnicodeChar, RestValues<CharCell>*>;
	using StringIteratorProcedureCell = TypedProcedureCell<UnicodeChar::CodePoint, UnicodeChar, RestValues<CharCell>*>;

	template<typename MapFunction>
	VectorCell* abstractVectorMap(World &world, MapFunction mapFunc, VectorCell *firstVectorRaw, RestValues<VectorCell> *restVectorList)
	{
		// This is the minimum length of all of our input vectors
		VectorCell::LengthType minimumLength = firstVectorRaw->length();

		for(auto restVector : *restVectorList)
		{
			minimumLength = std::min(minimumLength, restVector->length());
		}

		// Build our vector of input vector cells
		alloc::StrongRefVector<VectorCell> restVectors(world, restVectorList->begin(), restVectorList->end());

		// Root the input vector
		alloc::StrongRef<VectorCell> firstVector(world, firstVectorRaw);

		// Create the output vector and GC root it
		alloc::VectorRef outputVector(world, VectorCell::fromFill(world, minimumLength, UnitCell::instance()));

		std::vector<AnyCell*> restArgVector(restVectors.size());
		for(VectorCell::LengthType i = 0; i < minimumLength; i++)
		{
			// Build the rest argument list
			for(std::size_t j = 0; j < restVectors.size(); j++)
			{
				restArgVector[j] = restVectors[j]->elements()[i];
			}

			RestValues<AnyCell> *restArgList = RestValues<AnyCell>::create(world, restArgVector);
			AnyCell *result = mapFunc(firstVector->elements()[i], restArgList);

			// Use elements() here to skip the bounds check that setElementAt() will perform
			outputVector->elements()[i] = result;
		}

		return outputVector;
	}

	template<typename MapFunction>
	alloc::StrongRefVector<AnyCell> abstractListMap(World &world, MapFunction mapFunc, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>> *restListsRaw)
	{
		alloc::StrongRef<ListElementCell> firstList(world, firstListRaw);
		alloc::StrongRefVector<ListElementCell> restLists(world);

		// This is the minimum length of all of our input lists first
		ProperList<AnyCell>::size_type minimumLength = firstListRaw->size();

		for(auto restList : *restListsRaw)
		{
			// Create the strong ref for the rest list
			restLists.push_back(restList);

			minimumLength = std::min(minimumLength, restList->size());
		}

		// Create the vector of output values
		alloc::StrongRefVector<AnyCell> outputVector(world, minimumLength, nullptr);

		std::vector<AnyCell*> restArgVector(restLists.size());
		for(ProperList<AnyCell>::size_type i = 0; i < minimumLength; i++)
		{
			// Build the rest argument list
			for(std::size_t j = 0; j < restLists.size(); j++)
			{
				auto restListPair = cell_unchecked_cast<PairCell>(restLists[j]);
				restArgVector[j] = restListPair->car();

				// Move this forward to the next element
				restLists[j] = cell_unchecked_cast<ListElementCell>(restListPair->cdr());
			}

			// Create the rest argument list
			RestValues<AnyCell> *restArgList = RestValues<AnyCell>::create(world, restArgVector);

			// Extract the first list value and move it forward
			auto firstListPair = cell_unchecked_cast<PairCell>(firstList.data());
			firstList.setData(cell_unchecked_cast<ListElementCell>(firstListPair->cdr()));

			outputVector[i] = mapFunc(firstListPair->car(), restArgList);
		}

		return outputVector;
	}

	template<typename MapFunction>
	StringCellBuilder abstractStringMap(World &world, MapFunction mapFunc, StringCell *firstString, RestValues<StringCell> *restStringList)
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

		std::vector<UnicodeChar> restArgVector(restCharVectors.size());
		for(std::size_t i = 0; i < minimumLength; i++)
		{
			// Build the rest argument list
			for(std::size_t j = 0; j < restCharVectors.size(); j++)
			{
				restArgVector[j] = restCharVectors[j][i];
			}

			// Create the rest argument list
			RestValues<CharCell> *restArgList = RestValues<CharCell>::emplaceValues(world, restArgVector);

			builder << UnicodeChar(mapFunc(firstCharVector[i], restArgList));
		}

		return builder;
	}
}

extern "C"
{

VectorCell *llbase_vector_map(World &world, AnyMapProcedureCell *mapProcRaw, VectorCell *firstVectorRaw, RestValues<VectorCell> *argHead)
{
	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		return mapProc->apply(world, firstArg, restArgs);
	};

	return abstractVectorMap(world, mapFunc, firstVectorRaw, argHead);
}

void llbase_vector_for_each(World &world, AnyIteratorProcedureCell *mapProcRaw, VectorCell *firstVectorRaw, RestValues<VectorCell> *argHead)
{
	alloc::StrongRef<AnyIteratorProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		mapProc->apply(world, firstArg, restArgs);
		return UnitCell::instance();
	};

	abstractVectorMap(world, mapFunc, firstVectorRaw, argHead);
}

ProperList<AnyCell> *llbase_map(World &world, AnyMapProcedureCell *mapProcRaw, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>>* argHead)
{
	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		return mapProc->apply(world, firstArg, restArgs);
	};

	alloc::StrongRefVector<AnyCell> result = abstractListMap(world, mapFunc, firstListRaw, argHead);
	return ProperList<AnyCell>::create(world, result);
}

void llbase_for_each(World &world, AnyIteratorProcedureCell *mapProcRaw, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>>* argHead)
{
	alloc::StrongRef<AnyIteratorProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		mapProc->apply(world, firstArg, restArgs);
		return UnitCell::instance();
	};

	abstractListMap(world, mapFunc, firstListRaw, argHead);
}

StringCell *llbase_string_map(World &world, StringMapProcedureCell *mapProcRaw, StringCell *firstString, RestValues<StringCell> *argHead)
{
	alloc::StrongRef<StringMapProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (UnicodeChar firstArg, RestValues<CharCell> *restArgs) {
		return mapProc->apply(world, firstArg, restArgs);
	};

	StringCellBuilder builder(abstractStringMap(world, mapFunc, firstString, argHead));
	return builder.result(world);
}

void llbase_string_for_each(World &world, StringIteratorProcedureCell *mapProcRaw, StringCell *firstString, RestValues<StringCell> *argHead)
{
	alloc::StrongRef<StringIteratorProcedureCell> mapProc(world, mapProcRaw);

	auto mapFunc = [&] (UnicodeChar firstArg, RestValues<CharCell> *restArgs) {
		mapProc->apply(world, firstArg, restArgs);
		return UnicodeChar(0);
	};

	abstractStringMap(world, mapFunc, firstString, argHead);
}

}
