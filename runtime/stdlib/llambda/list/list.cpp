#include <vector>

#include <sstream>

#include "binding/ListElementCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"
#include "binding/TypedProcedureCell.h"

#include "alloc/StrongRefVector.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	struct SplitResult
	{
		ProperList<AnyCell> *head;
		AnyCell *tail;
	};

	SplitResult splitList(World &world, const char *procName, AnyCell *obj, std::uint32_t count)
	{
		AnyCell *tail = obj;
		alloc::StrongRefVector<AnyCell> headElements(world);

		while(count--)
		{
			auto pairTail = cell_cast<PairCell>(tail);

			if (pairTail == nullptr)
			{
				std::ostringstream message;
				message << procName << " on list of insufficient length";

				signalError(world, ErrorCategory::Range, message.str());
			}

			headElements.push_back(pairTail->car());
			tail = cell_unchecked_cast<AnyCell>(pairTail->cdr());
		}

		// Root our tail while we allocate the head list
		alloc::StrongRoot<AnyCell> tailRoot(world, &tail);

		// Build the head list
		ProperList<AnyCell> *head = ProperList<AnyCell>::create(world, headElements);

		return {head, tail};
	}

	struct SpanResult
	{
		ProperList<AnyCell> *head;
		ProperList<AnyCell> *tail;
	};

	template<typename T>
	SpanResult spanList(World &world, const char *procName, ProperList<AnyCell> *list, T predicate)
	{
		alloc::StrongRef<ProperList<AnyCell>> tail(world, list);
		alloc::StrongRefVector<AnyCell> headElements(world);

		while(true)
		{
			auto pairTail = cell_cast<PairCell>(tail);

			if (pairTail == nullptr)
			{
				// Ran out of list
				break;
			}

			if (!predicate(pairTail->car()))
			{
				// Predicate failed
				break;
			}

			headElements.push_back(pairTail->car());
			tail = cell_unchecked_cast<ProperList<AnyCell>>(pairTail->cdr());
		}

		// Build the head list
		ProperList<AnyCell> *head = ProperList<AnyCell>::create(world, headElements);

		return {head, tail};
	}

	/**
	 * Applies a procedure with the head values from a series of input lists and advances the lists
	 *
	 * If any of the lists have reached their end then this will return false and not apply the procedure. The input
	 * lists are of type AnyCell to reflect that this function and its callers should be prepared to have its input
	 * lists mutated to improper lists by the passed procedure.
	 *
	 * @param  world      World to apply the procedure in
	 * @param  firstList  Reference to the first proper list of values. This will be advanced to the next element.
	 * @param  restLists  Vector of other value proper lists. These will be advanced to their next elements.
	 * @param  proc       Procedure to apply. This will be passed one argument from each of the input lists.
	 * @param  result     Pointer to a location to store the result value. If this function returns false then the
	 *                    result will not be written to.
	 * @return Boolean indicating if all of the input lists were non-empty and the procedure was invoked.
	 */
	template<typename T>
	bool consumeInputLists(World &world, alloc::StrongRef<AnyCell> &firstList, alloc::StrongRefVector<AnyCell> &restLists, alloc::StrongRef<TypedProcedureCell<T, AnyCell*, RestValues<AnyCell>*>> proc, T* result)
	{
		auto firstPair = cell_cast<PairCell>(firstList);

		if (firstPair == nullptr)
		{
			// Reached end of the first list
			return false;
		}

		// Advance the list and store the head value
		firstList.setData(cell_unchecked_cast<ProperList<AnyCell>>(firstPair->cdr()));
		AnyCell *firstValue = firstPair->car();

		std::vector<AnyCell*> restValues(restLists.size());

		for(std::size_t i = 0; i < restLists.size(); i++)
		{
			auto restPair = cell_cast<PairCell>(restLists[i]);

			if (restPair == nullptr)
			{
				return false;
			}

			// Advance the list and store the head value
			restLists[i] = cell_unchecked_cast<ProperList<AnyCell>>(restPair->cdr());
			restValues[i] = restPair->car();
		}

		// Build the rest argument list
		RestValues<AnyCell> *restArgList = RestValues<AnyCell>::create(world, restValues);

		// Apply the function
		*result = proc->apply(world, firstValue, restArgList);

		return true;
	}
}

extern "C"
{

using PredicateProc = TypedProcedureCell<bool, AnyCell*>;
using FoldProc = TypedProcedureCell<AnyCell*, AnyCell*, AnyCell*, RestValues<AnyCell>*>;
using TabulateProc = TypedProcedureCell<AnyCell*, std::int64_t>;

using AnyProc = TypedProcedureCell<AnyCell*, AnyCell*, RestValues<AnyCell>*>;
using EveryProc = AnyProc;
using CountProc = AnyProc;

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

ProperList<AnyCell>* lllist_list_tabulate(World &world, std::uint32_t count, TabulateProc *initProcRaw)
{
	alloc::StrongRef<TabulateProc> initProc(world, initProcRaw);
	alloc::StrongRefVector<AnyCell> resultVec(world);

	for(std::uint32_t i = 0; i < count; i++)
	{
		resultVec.push_back(initProc->apply(world, i));
	}

	return ProperList<AnyCell>::create(world, resultVec);
}

ProperList<AnyCell>* lllist_take(World &world, AnyCell *obj, std::uint32_t count)
{
	return splitList(world, "(take)", obj, count).head;
}

AnyCell *lllist_drop(World &world, AnyCell *obj, std::uint32_t count)
{
	// Avoid splitList because it will allocate the head list which we don't use
	AnyCell *tail = obj;

	while(count--)
	{
		auto pairTail = cell_cast<PairCell>(tail);

		if (pairTail == nullptr)
		{
			signalError(world, ErrorCategory::Range, "(drop) on list of insufficient length");
		}

		tail = cell_unchecked_cast<AnyCell>(pairTail->cdr());
	}

	return tail;
}

ReturnValues<AnyCell>* lllist_split_at(World &world, AnyCell *obj, std::uint32_t count)
{
	SplitResult result = splitList(world, "(split-at)", obj, count);
	return ReturnValues<AnyCell>::create(world, {result.head, result.tail});
}

ReturnValues<ProperList<AnyCell>>* lllist_span(World &world, PredicateProc *predicateProcRaw, ProperList<AnyCell> *list)
{
	alloc::StrongRef<PredicateProc> predicateProc(world, predicateProcRaw);

	SpanResult result = spanList(world, "(span)", list, [&] (AnyCell *datum) {
		return predicateProc->apply(world, datum);
	});

	return ReturnValues<ProperList<AnyCell>>::create(world, {result.head, result.tail});
}

ReturnValues<ProperList<AnyCell>>* lllist_break(World &world, PredicateProc *predicateProcRaw, ProperList<AnyCell> *list)
{
	alloc::StrongRef<PredicateProc> predicateProc(world, predicateProcRaw);

	SpanResult result = spanList(world, "(break)", list, [&] (AnyCell *datum) {
		return !predicateProc->apply(world, datum);
	});

	return ReturnValues<ProperList<AnyCell>>::create(world, {result.head, result.tail});
}

AnyCell* lllist_any(World &world, AnyProc *predicateProcRaw, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>> *restListsRaw)
{
	// GC root everything
	alloc::StrongRef<AnyProc> predicateProc(world, predicateProcRaw);

	alloc::StrongRef<AnyCell> firstList(world, firstListRaw);
	alloc::StrongRefVector<AnyCell> restLists(world, restListsRaw->begin(), restListsRaw->end());

	// Run until we get a truth-y value
	while(true)
	{
		AnyCell *resultValue;

		if (!consumeInputLists<AnyCell*>(world, firstList, restLists, predicateProc, &resultValue))
		{
			// Ran out of lists
			return BooleanCell::falseInstance();
		}

		if (resultValue != BooleanCell::falseInstance())
		{
			// Found a non-false value
			return resultValue;
		}
	}
}

AnyCell* lllist_every(World &world, EveryProc *predicateProcRaw, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>> *restListsRaw)
{
	alloc::StrongRef<EveryProc> predicateProc(world, predicateProcRaw);

	alloc::StrongRef<AnyCell> firstList(world, firstListRaw);
	alloc::StrongRefVector<AnyCell> restLists(world, restListsRaw->begin(), restListsRaw->end());

	// If all lists are empty we should return #t
	AnyCell *resultValue = BooleanCell::trueInstance();

	// Run until we get a false value
	while(true)
	{
		if (!consumeInputLists<AnyCell*>(world, firstList, restLists, predicateProc, &resultValue))
		{
			// Ran out of lists - return the last result value
			// This depends on consumeInputLists not modifying resultValue when it returns false
			return resultValue;
		}

		if (resultValue == BooleanCell::falseInstance())
		{
			// Found a false value - we're false
			return BooleanCell::falseInstance();
		}
	}
}

std::int64_t lllist_count(World &world, CountProc *predicateProcRaw, ProperList<AnyCell> *firstListRaw, RestValues<ProperList<AnyCell>> *restListsRaw)
{
	alloc::StrongRef<CountProc> predicateProc(world, predicateProcRaw);

	alloc::StrongRef<AnyCell> firstList(world, firstListRaw);
	alloc::StrongRefVector<AnyCell> restLists(world, restListsRaw->begin(), restListsRaw->end());

	std::int64_t counter = 0;

	while(true)
	{
		AnyCell *resultValue;

		if (!consumeInputLists<AnyCell*>(world, firstList, restLists, predicateProc, &resultValue))
		{
			// Out of lists
			return counter;
		}

		if (resultValue != BooleanCell::falseInstance())
		{
			counter++;
		}
	}
}

}
