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

		auto *nextHead = cell_cast<ListElementCell>(headPair->cdr());

		if (nextHead == nullptr)
		{
			signalError(world, "Input list mutated during (partition)");
		}

		listHead.setData(nextHead);
	}

	alloc::StrongRef<ProperList<AnyCell>> trueList(world, ProperList<AnyCell>::create(world, trueValues));
	ProperList<AnyCell> *falseList(ProperList<AnyCell>::create(world, falseValues));

	return ReturnValues<ProperList<AnyCell>>::create(world, {trueList, falseList});
}

}
