#include <cassert>

#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	// This is used to implement memv and member without a callback
	const AnyCell* listSearch(const AnyCell *obj, ProperList<AnyCell> *listHead, bool (AnyCell::*equalityCheck)(const AnyCell*) const)
	{
		const AnyCell *cell = listHead;

		while(auto pair = cell_cast<PairCell>(cell))
		{
			if ((pair->car()->*equalityCheck)(obj))
			{
				return pair;
			}

			cell = pair->cdr();
		}

		assert(cell == EmptyListCell::instance());
		return BooleanCell::falseInstance();
	}

	// This is used to implement assv and assoc
	const AnyCell* alistSearch(const AnyCell *obj, ProperList<AnyCell> *listHead, bool (AnyCell::*equalityCheck)(const AnyCell*) const)
	{
		const AnyCell *cell = listHead;

		while(auto pair = cell_cast<PairCell>(cell))
		{
			auto testingPair = cell_unchecked_cast<PairCell>(pair->car());

			if ((testingPair->car()->*equalityCheck)(obj))
			{
				return testingPair;
			}

			cell = pair->cdr();
		}

		assert(cell == EmptyListCell::instance());
		return BooleanCell::falseInstance();
	}
}

extern "C"
{

PairCell *llbase_cons(World &world, AnyCell *car, AnyCell *cdr)
{
	return PairCell::createInstance(world, car, cdr);
}

AnyCell *llbase_car(PairCell *pair)
{
	return pair->car();
}

AnyCell *llbase_cdr(PairCell *pair)
{
	return pair->cdr();
}

std::uint32_t llbase_length(ProperList<AnyCell> *list)
{
	return list->size();
}

ListElementCell* llbase_make_list(World &world, std::uint32_t count, AnyCell *fill)
{
	ListElementCell *cdr = EmptyListCell::instance();
	std::uint32_t tailSize = 0;

	// Allocate all the new pairs at once
	alloc::RangeAlloc allocation(alloc::allocateRange(world, count));
	auto allocIt = allocation.end();

	while(allocIt != allocation.begin())
	{
		cdr = new (*--allocIt) PairCell(fill, cdr, ++tailSize);
	}

	return cdr;
}

AnyCell* llbase_append(World &world, RestValues<AnyCell> *argList)
{
	auto argCount = argList->size();

	if (argCount == 0)
	{
		// Nothing to append
		return EmptyListCell::instance();
	}

	std::vector<AnyCell*> appendedElements;
	auto argIt = argList->begin();

	while(--argCount)
	{
		auto argDatum = *(argIt++);

		if (auto pair = cell_cast<PairCell>(argDatum))
		{
			// Zero means "unknown list length" which will cause this to be a no-op
			appendedElements.reserve(appendedElements.size() + pair->listLength());

			do
			{
				appendedElements.insert(appendedElements.end(), pair->car());
				argDatum = pair->cdr();
			}
			while((pair = cell_cast<PairCell>(argDatum)));
		}

		if (argDatum != EmptyListCell::instance())
		{
			signalError(world, ErrorCategory::Type, "Non-list passed to (append) in non-terminal position", {argDatum});
		}
	}

	// Use createList to append the last list on sharing its structure. This is required by R7RS
	return ListElementCell::createList(world, appendedElements, *(argIt++));
}

ProperList<AnyCell>* llbase_reverse(World &world, ProperList<AnyCell> *sourceList)
{
	alloc::RangeAlloc allocation = alloc::allocateRange(world, sourceList->size());
	auto allocIt = allocation.end();

	AnyCell *cdr = EmptyListCell::instance();
	for(auto car : *sourceList)
	{
		cdr = new (*--allocIt) PairCell(car, cdr);
	}

	return static_cast<ProperList<AnyCell>*>(cdr);
}

const AnyCell* llbase_memv(const AnyCell *obj, ProperList<AnyCell> *listHead)
{
	return listSearch(obj, listHead, &AnyCell::isEqv);
}

const AnyCell* llbase_member(const AnyCell *obj, ProperList<AnyCell> *listHead)
{
	return listSearch(obj, listHead, &AnyCell::isEqual);
}

const AnyCell* llbase_assv(const AnyCell *obj, ProperList<AnyCell> *listHead)
{
	return alistSearch(obj, listHead, &AnyCell::isEqv);
}

const AnyCell* llbase_assoc(const AnyCell *obj, ProperList<AnyCell> *listHead)
{
	return alistSearch(obj, listHead, &AnyCell::isEqual);
}

ListElementCell* llbase_list_tail(World &world, ProperList<AnyCell> *initialHead, std::uint32_t count)
{
	ListElementCell *head = initialHead;

	while(count--)
	{
		auto pairHead = cell_cast<PairCell>(head);

		if (pairHead == nullptr)
		{
			signalError(world, ErrorCategory::Range, "(list-tail) on list of insufficient length");
		}

		// Advance to the next list element
		// Our argument is defined to be a proper list on the Scheme side so this is safe
		head = cell_unchecked_cast<ListElementCell>(pairHead->cdr());
	}

	return head;
}

}
