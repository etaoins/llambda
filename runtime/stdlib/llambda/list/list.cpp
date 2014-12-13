#include <vector>

#include "binding/ListElementCell.h"
#include "binding/ProperList.h"
#include "binding/TypedProcedureCell.h"

#include "alloc/StrongRefVector.h"

#include "core/error.h"

using namespace lliby;

extern "C"
{

using PredicateProc = TypedProcedureCell<bool, AnyCell*>;
using FoldProc = TypedProcedureCell<AnyCell*, AnyCell*, AnyCell*, RestValues<AnyCell>*>;

AnyCell *lllist_cons_star(World &world, AnyCell *headValue, RestValues<AnyCell> *tailValues)
{
	const auto elementCount = tailValues->size() + 1;

	if (elementCount == 1)
	{
		// This is easy
		return headValue;
	}

	std::vector<AnyCell*> nonTerminalElements(elementCount - 1);

	nonTerminalElements[0] = headValue;

	auto tailArgIt = tailValues->begin();
	for(RestValues<AnyCell>::size_type i = 1; i < (elementCount - 1); i++)
	{
		nonTerminalElements[i] = *(tailArgIt++);
	}

	return ListElementCell::createList(world, nonTerminalElements, *tailArgIt);
}

AnyCell* lllist_fold(World &world, FoldProc *foldProcRaw, AnyCell *initialValue, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>> *restListsRaw)
{
	alloc::StrongRef<FoldProc> foldProc(world, foldProcRaw);
	alloc::StrongRef<AnyCell> accum(world, initialValue);

	// Collect our input lists
	alloc::StrongRefVector<ListElementCell> inputLists(world);
	inputLists.push_back(firstListRaw);

	for(auto restList : *restListsRaw)
	{
		// Create the strong ref for the rest list
		inputLists.push_back(restList);
	}

	const auto inputListCount = inputLists.size();

	std::vector<AnyCell*> inputVector(inputListCount + 1);
	std::vector<AnyCell*> restArgVector(inputListCount - 1);

	while(true)
	{
		// Collect our input from out input lists
		for(size_t i = 0; i < inputListCount; i++)
		{
			if (inputLists[i] == EmptyListCell::instance())
			{
				// Reached the end of the list
				return accum;
			}

			auto inputListPair = cell_unchecked_cast<PairCell>(inputLists[i]);
			inputVector[i] = inputListPair->car();

			// Move this forward to the next element
			inputLists[i] = cell_checked_cast<ListElementCell>(world, inputListPair->cdr(), "Input list mutated during (fold)");
		}

		inputVector[inputListCount] = accum;

		// Create the rest argument list - the first two input values are passed explicitly
		std::copy(inputVector.begin() + 2, inputVector.end(), restArgVector.begin());
		RestValues<AnyCell> *restArgList = RestValues<AnyCell>::create(world, restArgVector);

		auto resultValue = foldProc->apply(world, inputVector[0], inputVector[1], restArgList);
		accum.setData(resultValue);
	}
}

ReturnValues<ProperList<AnyCell>>* lllist_partition(World &world, PredicateProc *predicateProcRaw, ProperList<AnyCell> *listHeadRaw)
{
	alloc::StrongRef<PredicateProc> predicateProc(world, predicateProcRaw);
	alloc::StrongRef<ListElementCell> listHead(world, listHeadRaw);

	alloc::StrongRefVector<AnyCell> trueValues(world);
	alloc::StrongRefVector<AnyCell> falseValues(world);

	while(listHead != EmptyListCell::instance())
	{
		// This must be a pair if we're not the empty list and the predicate hasn't been called yet
		auto headPair = cell_unchecked_cast<PairCell>(listHead);
		AnyCell *headValue = headPair->car();

		if (predicateProc->apply(world, headValue))
		{
			trueValues.push_back(headValue);
		}
		else
		{
			falseValues.push_back(headValue);
		}

		auto *nextHead = cell_checked_cast<ListElementCell>(world, headPair->cdr(), "Input list mutated during (partition)");
		listHead.setData(nextHead);
	}

	alloc::StrongRef<ProperList<AnyCell>> trueList(world, ProperList<AnyCell>::create(world, trueValues));
	ProperList<AnyCell> *falseList(ProperList<AnyCell>::create(world, falseValues));

	return ReturnValues<ProperList<AnyCell>>::create(world, {trueList, falseList});
}

}
