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

	template<typename InitFunction, typename IterFunction, typename FinalFunction>
	auto abstractVectorIter(World &world, InitFunction initFunc, IterFunction iterFunc, FinalFunction finalFunc, VectorCell *firstVectorRaw, RestValues<VectorCell> *restVectorList)
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

		auto container = initFunc(minimumLength);

		std::vector<AnyCell*> restArgVector(restVectors.size());
		for(VectorCell::LengthType i = 0; i < minimumLength; i++)
		{
			// Build the rest argument list
			for(std::size_t j = 0; j < restVectors.size(); j++)
			{
				restArgVector[j] = restVectors[j]->elements()[i];
			}

			RestValues<AnyCell> *restArgList = RestValues<AnyCell>::create(world, restArgVector);
			iterFunc(container, i, firstVector->elements()[i], restArgList);
		}

		// This finalFunc() is used because NRVO seems to break when the container is returned from initFunc() in
		// Clang/LLVM 3.9. This allows us to return a cell pointer instead of a complex accumulator object.
		return finalFunc(container);
	}

	template<typename InitFunction, typename IterFunction, typename FinalFunction>
	auto abstractListIter(World &world, InitFunction initFunc, IterFunction iterFunc, FinalFunction finalFunc, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>> *restListsRaw)
	{
		alloc::StrongRef<ListElementCell> firstList(world, firstListRaw);
		alloc::StrongRefVector<ListElementCell> restLists(world);

		// This is the minimum length of all of our input lists
		ProperList<AnyCell>::size_type minimumLength = firstListRaw->size();

		for(auto restList : *restListsRaw)
		{
			// Create the strong ref for the rest list
			restLists.push_back(restList);

			minimumLength = std::min(minimumLength, restList->size());
		}

		auto container = initFunc(minimumLength);

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

			iterFunc(container, i, firstListPair->car(), restArgList);
		}

		return finalFunc(container);
	}

	template<typename InitFunction, typename IterFunction, typename FinalFunction>
	auto abstractStringIter(World &world, InitFunction initFunc, IterFunction iterFunc, FinalFunction finalFunc, StringCell *firstString, RestValues<StringCell> *restStringList)
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

		auto container = initFunc(minimumLength);

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

			iterFunc(container, i, firstCharVector[i], restArgList);
		}

		return finalFunc(container);
	}
}

extern "C"
{

VectorCell *llbase_vector_map(World &world, AnyMapProcedureCell *mapProcRaw, VectorCell *firstVectorRaw, RestValues<VectorCell> *argHead)
{
	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	auto initFunc = [&] (VectorCell::LengthType capacity) {
		return alloc::VectorRef(world, VectorCell::fromFill(world, capacity, UnitCell::instance()));
	};

	auto iterFunc = [&] (alloc::VectorRef &outputVector, std::size_t i, AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		// Use elements() here to skip the bounds check that setElementAt() will perform
		outputVector->elements()[i] = mapProc->apply(world, firstArg, restArgs);
	};

	auto finalFunc = [&] (alloc::VectorRef &outputVector) {
		return outputVector.data();
	};

	return abstractVectorIter(world, initFunc, iterFunc, finalFunc, firstVectorRaw, argHead);
}

void llbase_vector_for_each(World &world, AnyIteratorProcedureCell *iterProcRaw, VectorCell *firstVectorRaw, RestValues<VectorCell> *argHead)
{
	alloc::StrongRef<AnyIteratorProcedureCell> iterProc(world, iterProcRaw);

	auto initFunc = [&] (VectorCell::LengthType) {
		return 0;
	};

	auto iterFunc = [&] (int, std::size_t, AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		iterProc->apply(world, firstArg, restArgs);
	};

	auto finalFunc = [&] (int) {
		return;
	};

	abstractVectorIter(world, initFunc, iterFunc, finalFunc, firstVectorRaw, argHead);
}

ProperList<AnyCell> *llbase_map(World &world, AnyMapProcedureCell *mapProcRaw, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>>* argHead)
{
	alloc::StrongRef<AnyMapProcedureCell> mapProc(world, mapProcRaw);

	auto initFunc = [&] (ProperList<AnyCell>::size_type capacity) {
		return alloc::StrongRefVector<AnyCell>(world, capacity, nullptr);
	};

	auto iterFunc = [&] (alloc::StrongRefVector<AnyCell> &outputVector, std::size_t i, AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		outputVector[i] = mapProc->apply(world, firstArg, restArgs);
	};

	auto finalFunc = [&] (alloc::StrongRefVector<AnyCell> &outputVector) {
		return ProperList<AnyCell>::create(world, outputVector);
	};

	return abstractListIter(world, initFunc, iterFunc, finalFunc, firstListRaw, argHead);
}

void llbase_for_each(World &world, AnyIteratorProcedureCell *iterProcRaw, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>>* argHead)
{
	alloc::StrongRef<AnyIteratorProcedureCell> iterProc(world, iterProcRaw);

	auto initFunc = [&] (ProperList<AnyCell>::size_type) {
		return 9;
	};

	auto iterFunc = [&] (int, std::size_t, AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		iterProc->apply(world, firstArg, restArgs);
	};

	auto finalFunc = [&] (int) {
		return;
	};

	abstractListIter(world, initFunc, iterFunc, finalFunc, firstListRaw, argHead);
}

StringCell *llbase_string_map(World &world, StringMapProcedureCell *mapProcRaw, StringCell *firstString, RestValues<StringCell> *argHead)
{
	alloc::StrongRef<StringMapProcedureCell> mapProc(world, mapProcRaw);

	auto initFunc = [&] (std::size_t capacity) {
		return StringCellBuilder(capacity);
	};

	auto iterFunc = [&] (StringCellBuilder &builder, std::size_t, UnicodeChar firstArg, RestValues<CharCell> *restArgs) {
		builder << UnicodeChar(mapProc->apply(world, firstArg, restArgs));
	};

	auto finalFunc = [&] (StringCellBuilder &builder) {
		return builder.result(world);
	};

	return abstractStringIter(world, initFunc, iterFunc, finalFunc, firstString, argHead);
}

void llbase_string_for_each(World &world, StringIteratorProcedureCell *iterProcRaw, StringCell *firstString, RestValues<StringCell> *argHead)
{
	alloc::StrongRef<StringIteratorProcedureCell> iterProc(world, iterProcRaw);

	auto initFunc = [&] (std::size_t) {
		return 0;
	};

	auto iterFunc = [&] (int, std::size_t, UnicodeChar firstArg, RestValues<CharCell> *restArgs) {
		iterProc->apply(world, firstArg, restArgs);
	};

	auto finalFunc = [&] (int) {
		return;
	};

	abstractStringIter(world, initFunc, iterFunc, finalFunc, firstString, argHead);
}

}
