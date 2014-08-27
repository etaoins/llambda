#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"
#include "binding/RestArgument.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"
#include "alloc/cellref.h"

#include "core/error.h"

using namespace lliby;

namespace 
{
	// This is used to implement memq, memv and member without a callback
	const AnyCell* list_search(World &world, const AnyCell *obj, ListElementCell *listHead, bool (AnyCell::*equalityCheck)(const AnyCell*) const)
	{
		const AnyCell *cell = listHead;

		// Do this in a single pass for efficiency
		// ProperList doesn't give us much here
		while(auto pair = cell_cast<PairCell>(cell))
		{
			if ((pair->car()->*equalityCheck)(obj))
			{
				return pair;
			}

			cell = pair->cdr();
		}

		if (cell == EmptyListCell::instance())
		{
			return BooleanCell::falseInstance();
		}
		else
		{
			signalError(world, "Attempted to search non-list", {listHead});
		}
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

std::uint32_t lliby_length(World &world, ListElementCell *head) 
{
	ProperList<AnyCell> properList(head);

	if (!properList.isValid())
	{
		signalError(world, "Non-list passed to list-length", {head});
	}

	return properList.length();
}

ListElementCell* lliby_make_list(World &world, std::uint32_t count, AnyCell *fill)
{
	ListElementCell *cdr = EmptyListCell::instance();

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

ListElementCell* lliby_list(RestArgument<AnyCell> *head)
{
	// Our calling convention requires that any rest parameters are passed as
	// a proper list. Because (list) is defined as only having rest args the
	// codegen will do the heavy lifting of building the list and we only have
	// to return it.
	return head;
}

AnyCell* lliby_append(World &world, RestArgument<AnyCell> *argHead)
{
	ProperList<AnyCell> argList(argHead);

	auto argCount = argList.length();

	if (argCount == 0)
	{
		// Nothing to append
		return EmptyListCell::instance();
	}

	// XXX: This is not very efficient
	std::vector<AnyCell*> appenedElements;
	size_t appendIndex = 0;

	auto argIt = argList.begin();

	while(--argCount)
	{
		auto argDatum = *(argIt++);
		auto listHead = cell_cast<ListElementCell>(argDatum);

		if (listHead == nullptr)
		{
			signalError(world, "Non-list passed to (append) in non-terminal position", {argDatum});
		}

		// Get the passed list
		ProperList<AnyCell> properList(listHead);
	
		// Reserve the size of the vector
		appenedElements.resize(appendIndex + properList.length());

		if (!properList.isValid())
		{
			signalError(world, "Improper list passed to (append) in non-terminal position", {listHead});
		}

		for(auto element : properList)
		{
			appenedElements[appendIndex++] = element;
		}
	}

	// Use createList to append the last list on sharing its structure.
	// This is required by R7RS
	return ListElementCell::createList(world, appenedElements, *(argIt++));
}

ListElementCell* lliby_reverse(World &world, ListElementCell *sourceHead)
{
	ProperList<AnyCell> sourceList(sourceHead);
	auto sourceIt = sourceList.begin(); 
	auto memberCount = sourceList.length();

	std::vector<AnyCell*> reversedMembers(memberCount);

	while(memberCount--)
	{
		reversedMembers[memberCount] = *(sourceIt++);
	}

	return ListElementCell::createProperList(world, reversedMembers);
}

const AnyCell* lliby_memv(World &world, const AnyCell *obj, ListElementCell *listHead)
{
	return list_search(world, obj, listHead, &AnyCell::isEqv);
}

const AnyCell* lliby_member(World &world, const AnyCell *obj, ListElementCell *listHead)
{
	return list_search(world, obj, listHead, &AnyCell::isEqual);
}

}
