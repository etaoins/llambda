#include <cassert>

#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"
#include "alloc/cellref.h"

#include "core/error.h"

using namespace lliby;

namespace
{
	// This is used to implement memq, memv and member without a callback
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

	// This is used to implement assq, assv and assoc
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

PairCell *lliby_cons(World &world, AnyCell *car, AnyCell *cdr)
{
	return PairCell::createInstance(world, car, cdr);
}

AnyCell *lliby_car(PairCell *pair)
{
	return pair->car();
}

AnyCell *lliby_cdr(PairCell *pair)
{
	return pair->cdr();
}

void lliby_set_car(World &world, PairCell *pair, AnyCell *obj)
{
	if (pair->isGlobalConstant())
	{
		signalError(world, "(set-car!) on pair literal", {pair});	
	}

	return pair->setCar(obj);
}

void lliby_set_cdr(World &world, PairCell *pair, AnyCell *obj)
{
	if (pair->isGlobalConstant())
	{
		signalError(world, "(set-cdr!) on pair literal", {pair});	
	}

	return pair->setCdr(obj);
}

std::uint32_t lliby_length(World &world, ProperList<AnyCell> *list)
{
	return list->size();
}

ListElementCell* lliby_make_list(World &world, std::uint32_t count, AnyCell *fillRaw)
{
	ListElementCell *cdr = EmptyListCell::instance();
	alloc::AnyRef fill(world, fillRaw);

	// Allocate all the new pairs at once
	alloc::RangeAlloc allocation(alloc::allocateRange(world, count));
	auto allocIt = allocation.end();

	while(allocIt != allocation.begin())
	{
		cdr = new (*--allocIt) PairCell(fill, cdr);
	}

	return cdr;
}

AnyCell* lliby_list_copy(World &world, AnyCell *sourceHead)
{
	// Find the number of pairs in the list
	// We can't use ProperList because we need to work with improper lists and non-list objects
	std::uint32_t pairCount = 0;

	for(auto pair = cell_cast<PairCell>(sourceHead);
		pair != nullptr;
		pair = cell_cast<PairCell>(pair->cdr()))
	{
		pairCount++;
	}

	if (pairCount == 0)
	{
		return sourceHead;
	}

	// Make sure we take a reference to this across the next allocation in case the GC runs
	alloc::AnyRef sourceHeadRef(world, sourceHead);	

	alloc::AllocCell *destHead = alloc::allocateCells(world, pairCount);
	alloc::AllocCell *destPair = destHead;

	// We've counted our pairs so this has to be a pair
	auto sourcePair = cell_unchecked_cast<const PairCell>(sourceHeadRef.data());

	// This is predecrement because the last pair is handled specially below this loop
	while(--pairCount)
	{
		// Create the new pair cdr'ed to the next pair
		new (destPair) PairCell(sourcePair->car(), destPair + 1);

		destPair++;

		// Move to the next pair
		sourcePair = cell_unchecked_cast<PairCell>(sourcePair->cdr());
	}
	
	// Place our last pair cdr'ed to the last cdr
	// For proper lists this is the empty list
	// For improper list this is another type of non-pair cell
	new (destPair) PairCell(sourcePair->car(), sourcePair->cdr());

	// All done!
	return destHead;
}

ProperList<AnyCell>* lliby_list(ProperList<AnyCell> *head)
{
	// Our calling convention requires that rest parameters are passed as a proper list. Because (list) is defined as
	// only having rest args the codegen will do the heavy lifting of building the list and we only have to return it.
	return head;
}

AnyCell* lliby_append(World &world, ProperList<AnyCell> *argList)
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
		auto properList = cell_cast<ProperList<AnyCell>>(argDatum);

		if (properList == nullptr)
		{
			signalError(world, "Non-list passed to (append) in non-terminal position", {argDatum});
		}

		appendedElements.insert(appendedElements.end(), properList->begin(), properList->end());
	}

	// Use createList to append the last list on sharing its structure. This is required by R7RS
	return ListElementCell::createList(world, appendedElements, *(argIt++));
}

ProperList<AnyCell>* lliby_reverse(World &world, ProperList<AnyCell> *sourceList)
{
	auto sourceIt = sourceList->begin();
	auto memberCount = sourceList->size();

	std::vector<AnyCell*> reversedMembers(memberCount);

	while(memberCount--)
	{
		reversedMembers[memberCount] = *(sourceIt++);
	}

	return ProperList<AnyCell>::create(world, reversedMembers);
}

const AnyCell* lliby_memv(const AnyCell *obj, ProperList<AnyCell> *listHead)
{
	return listSearch(obj, listHead, &AnyCell::isEqv);
}

const AnyCell* lliby_member(const AnyCell *obj, ProperList<AnyCell> *listHead)
{
	return listSearch(obj, listHead, &AnyCell::isEqual);
}

const AnyCell* lliby_assv(const AnyCell *obj, ProperList<AnyCell> *listHead)
{
	return alistSearch(obj, listHead, &AnyCell::isEqv);
}

const AnyCell* lliby_assoc(const AnyCell *obj, ProperList<AnyCell> *listHead)
{
	return alistSearch(obj, listHead, &AnyCell::isEqual);
}

ListElementCell* lliby_list_tail(World &world, ProperList<AnyCell> *initialHead, std::uint32_t count)
{
	ListElementCell *head = initialHead;

	while(count--)
	{
		auto pairHead = cell_cast<PairCell>(head);

		if (pairHead == nullptr)
		{
			signalError(world, "(list-tail) on list of insufficient length");
		}

		// Advance to the next list element
		// Our argument is defined to be a proper list on the Scheme side so this is safe
		head = cell_unchecked_cast<ListElementCell>(pairHead->cdr());
	}

	return head;
}

}
