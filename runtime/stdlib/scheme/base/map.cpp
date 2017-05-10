#include "binding/ProcedureCell.h"
#include "binding/TypedProcedureCell.h"
#include "binding/VectorCell.h"
#include "binding/CharCell.h"
#include "binding/StringCell.h"
#include "unicode/UnicodeChar.h"
#include "binding/ProperList.h"
#include "binding/UnitCell.h"

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
	auto abstractVectorIter(World &world, InitFunction initFunc, IterFunction iterFunc, FinalFunction finalFunc, VectorCell *firstVector, RestValues<VectorCell> *restVectorList)
	{
		// This is the minimum length of all of our input vectors
		VectorCell::LengthType minimumLength = firstVector->length();

		for(auto restVector : *restVectorList)
		{
			minimumLength = std::min(minimumLength, restVector->length());
		}

		// Build our vector of input vector cells
		std::vector<VectorCell*> restVectors(restVectorList->begin(), restVectorList->end());

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
	auto abstractListIter(World &world, InitFunction initFunc, IterFunction iterFunc, FinalFunction finalFunc, ProperList<AnyCell> *firstList, RestValues<ProperList<AnyCell>> *restListsRaw)
	{
		std::vector<ListElementCell*> restLists;

		// This is the minimum length of all of our input lists
		ProperList<AnyCell>::size_type minimumLength = firstList->size();

		for(auto restList : *restListsRaw)
		{
			restLists.push_back(restList);
			minimumLength = std::min(minimumLength, restList->size());
		}

		auto container = initFunc(minimumLength);

		std::vector<AnyCell*> restArgVector(restLists.size());
		ListElementCell *firstListHead = firstList;
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
			auto firstListPair = cell_unchecked_cast<PairCell>(firstListHead);
			firstListHead = cell_unchecked_cast<ListElementCell>(firstListPair->cdr());

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

VectorCell *llbase_vector_map(World &world, AnyMapProcedureCell *mapProc, VectorCell *firstVector, RestValues<VectorCell> *argHead)
{
	auto initFunc = [&] (VectorCell::LengthType capacity) {
		return VectorCell::fromFill(world, capacity, UnitCell::instance());
	};

	auto iterFunc = [&] (VectorCell *outputVector, std::size_t i, AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		// Use elements() here to skip the bounds check that setElementAt() will perform
		outputVector->elements()[i] = mapProc->apply(world, firstArg, restArgs);
	};

	auto finalFunc = [&] (VectorCell *outputVector) {
		return outputVector;
	};

	return abstractVectorIter(world, initFunc, iterFunc, finalFunc, firstVector, argHead);
}

void llbase_vector_for_each(World &world, AnyIteratorProcedureCell *iterProc, VectorCell *firstVector, RestValues<VectorCell> *argHead)
{
	auto initFunc = [&] (VectorCell::LengthType) {
		return 0;
	};

	auto iterFunc = [&] (int, std::size_t, AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		iterProc->apply(world, firstArg, restArgs);
	};

	auto finalFunc = [&] (int) {
		return;
	};

	abstractVectorIter(world, initFunc, iterFunc, finalFunc, firstVector, argHead);
}

ProperList<AnyCell> *llbase_map(World &world, AnyMapProcedureCell *mapProc, ProperList<AnyCell> *firstList, RestValues<ProperList<AnyCell>>* argHead)
{
	auto initFunc = [&] (ProperList<AnyCell>::size_type capacity) {
		return std::vector<AnyCell*>(capacity, nullptr);
	};

	auto iterFunc = [&] (std::vector<AnyCell*> &outputVector, std::size_t i, AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		outputVector[i] = mapProc->apply(world, firstArg, restArgs);
	};

	auto finalFunc = [&] (std::vector<AnyCell*> &outputVector) {
		return ProperList<AnyCell>::create(world, outputVector);
	};

	return abstractListIter(world, initFunc, iterFunc, finalFunc, firstList, argHead);
}

void llbase_for_each(World &world, AnyIteratorProcedureCell *iterProc, ProperList<AnyCell> *firstList, RestValues<ProperList<AnyCell>>* argHead)
{
	auto initFunc = [&] (ProperList<AnyCell>::size_type) {
		return 9;
	};

	auto iterFunc = [&] (int, std::size_t, AnyCell *firstArg, RestValues<AnyCell> *restArgs) {
		iterProc->apply(world, firstArg, restArgs);
	};

	auto finalFunc = [&] (int) {
		return;
	};

	abstractListIter(world, initFunc, iterFunc, finalFunc, firstList, argHead);
}

StringCell *llbase_string_map(World &world, StringMapProcedureCell *mapProc, StringCell *firstString, RestValues<StringCell> *argHead)
{
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

void llbase_string_for_each(World &world, StringIteratorProcedureCell *iterProc, StringCell *firstString, RestValues<StringCell> *argHead)
{
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
