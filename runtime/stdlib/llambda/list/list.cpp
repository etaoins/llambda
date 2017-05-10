#include <vector>

#include <sstream>

#include "binding/ListElementCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"
#include "binding/TypedProcedureCell.h"
#include "binding/FlonumCell.h"
#include "binding/IntegerCell.h"

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
		std::vector<AnyCell*> headElements;

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
		ProperList<AnyCell> *tail = list;
		std::vector<AnyCell*> headElements;

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
	bool consumeInputLists(World &world, ProperList<lliby::AnyCell>* &firstList, std::vector<AnyCell*> &restLists, TypedProcedureCell<T, AnyCell*, RestValues<AnyCell>*> *proc, T* result)
	{
		auto firstPair = cell_cast<PairCell>(firstList);

		if (firstPair == nullptr)
		{
			// Reached end of the first list
			return false;
		}

		// Advance the list and store the head value
		firstList = cell_unchecked_cast<ProperList<AnyCell>>(firstPair->cdr());
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
using AppendMapProc = TypedProcedureCell<ProperList<AnyCell>*, AnyCell*, RestValues<AnyCell>*>;
using FilterMapProc = TypedProcedureCell<AnyCell*, AnyCell*, RestValues<AnyCell>*>;

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

AnyCell* lllist_fold(World &world, FoldProc *foldProc, AnyCell *initialValue, ProperList<AnyCell> *firstList, RestValues<ProperList<AnyCell>> *restLists)
{
	AnyCell *accum = initialValue;

	// Collect our input lists
	std::vector<ListElementCell*> inputLists;
	inputLists.push_back(firstList);

	for(auto restList : *restLists)
	{
		inputLists.push_back(restList);
	}

	const auto inputListCount = inputLists.size();

	std::vector<AnyCell*> inputVector(inputListCount + 1);
	std::vector<AnyCell*> restArgVector(inputListCount - 1);

	while(true)
	{
		// Collect our input from out input lists
		for(std::size_t i = 0; i < inputListCount; i++)
		{
			if (inputLists[i] == EmptyListCell::instance())
			{
				// Reached the end of the list
				return accum;
			}

			auto inputListPair = cell_unchecked_cast<PairCell>(inputLists[i]);
			inputVector[i] = inputListPair->car();

			// Move this forward to the next element
			inputLists[i] = cell_unchecked_cast<ListElementCell>(inputListPair->cdr());
		}

		inputVector[inputListCount] = accum;

		// Create the rest argument list - the first two input values are passed explicitly
		std::copy(inputVector.begin() + 2, inputVector.end(), restArgVector.begin());
		RestValues<AnyCell> *restArgList = RestValues<AnyCell>::create(world, restArgVector);

		auto resultValue = foldProc->apply(world, inputVector[0], inputVector[1], restArgList);
		accum = resultValue;
	}
}

PairCell* lllist_partition(World &world, PredicateProc *predicateProc, ProperList<AnyCell> *list)
{
	ListElementCell *listHead = list;

	std::vector<AnyCell*> trueValues;
	std::vector<AnyCell*> falseValues;

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

		auto *nextHead = cell_unchecked_cast<ListElementCell>(headPair->cdr());
		listHead = nextHead;
	}

	ProperList<AnyCell> *trueList(ProperList<AnyCell>::create(world, trueValues));
	ProperList<AnyCell> *falseList(ProperList<AnyCell>::create(world, falseValues));

	return PairCell::createInstance(world, trueList, falseList);
}

ProperList<AnyCell>* lllist_list_tabulate(World &world, std::uint32_t count, TabulateProc *initProc)
{
	std::vector<AnyCell*> resultVec;

	resultVec.reserve(count);

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

PairCell* lllist_split_at(World &world, AnyCell *obj, std::uint32_t count)
{
	SplitResult result = splitList(world, "(split-at)", obj, count);
	return PairCell::createInstance(world, result.head, result.tail);
}

PairCell* lllist_span(World &world, PredicateProc *predicateProc, ProperList<AnyCell> *list)
{
	SpanResult result = spanList(world, "(span)", list, [&] (AnyCell *datum) {
		return predicateProc->apply(world, datum);
	});

	return PairCell::createInstance(world, result.head, result.tail);
}

PairCell* lllist_break(World &world, PredicateProc *predicateProc, ProperList<AnyCell> *list)
{
	SpanResult result = spanList(world, "(break)", list, [&] (AnyCell *datum) {
		return !predicateProc->apply(world, datum);
	});

	return PairCell::createInstance(world, result.head, result.tail);
}

AnyCell* lllist_any(World &world, AnyProc *predicateProc, ProperList<AnyCell> *firstList, RestValues<ProperList<AnyCell>> *restListsRaw)
{
	std::vector<AnyCell*> restLists(restListsRaw->begin(), restListsRaw->end());

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

AnyCell* lllist_every(World &world, EveryProc *predicateProc, ProperList<AnyCell> *firstList, RestValues<ProperList<AnyCell>> *restListsRaw)
{
	std::vector<AnyCell*> restLists(restListsRaw->begin(), restListsRaw->end());

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

std::int64_t lllist_count(World &world, CountProc *predicateProc, ProperList<AnyCell> *firstList, RestValues<ProperList<AnyCell>> *restListsRaw)
{
	std::vector<AnyCell*> restLists(restListsRaw->begin(), restListsRaw->end());

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

ProperList<AnyCell>* lllist_append_map(World &world, AppendMapProc *mapProc, ProperList<AnyCell> *firstList, RestValues<ProperList<AnyCell>> *restListsRaw)
{
	std::vector<AnyCell*> restLists(restListsRaw->begin(), restListsRaw->end());

	std::vector<AnyCell*> resultValues;

	ProperList<AnyCell> *resultList;
	while(consumeInputLists(world, firstList, restLists, mapProc, &resultList))
	{
		// Splice this list on to the results
		resultValues.insert(resultValues.end(), resultList->begin(), resultList->end());
	}

	return ProperList<AnyCell>::create(world, resultValues);
}

ProperList<AnyCell>* lllist_filter_map(World &world, FilterMapProc *mapProc, ProperList<AnyCell> *firstList, RestValues<ProperList<AnyCell>> *restListsRaw)
{
	std::vector<AnyCell*> restLists(restListsRaw->begin(), restListsRaw->end());

	std::vector<AnyCell*> resultValues;

	AnyCell *resultValue;
	while(consumeInputLists(world, firstList, restLists, mapProc, &resultValue))
	{
		if (resultValue != BooleanCell::falseInstance())
		{
			// Add this value to the results
			resultValues.push_back(resultValue);
		}
	}

	return ProperList<AnyCell>::create(world, resultValues);
}

ListElementCell* lllist_iota(World &world, std::uint32_t count, NumberCell *startCell, NumberCell *stepCell)
{
	if (auto startIntegerCell = cell_cast<IntegerCell>(startCell))
	{
		if (auto stepIntegerCell = cell_cast<IntegerCell>(stepCell))
		{
			std::uint32_t start = startIntegerCell->value();
			std::uint32_t step = stepIntegerCell->value();

			std::vector<std::uint32_t> values(count);

			for(std::uint32_t i = 0; i < count; i++)
			{
				values[i] = start + (step * i);
			}

			return ProperList<IntegerCell>::emplaceValues(world, values);
		}
	}

	double start = startCell->toDouble();
	double step = stepCell->toDouble();

	std::vector<double> values(count);

	for(std::uint32_t i = 0; i < count; i++)
	{
		values[i] = start + (step * i);
	}

	return ProperList<FlonumCell>::emplaceValues(world, values);
}

}
