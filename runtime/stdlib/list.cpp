#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"

#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"
#include "alloc/StrongRef.h"

#include "core/error.h"

using namespace lliby;

namespace 
{
	// This is used to implement memq, memv and member without a callback
	const DatumCell* list_search(World &world, const DatumCell *obj, ListElementCell *listHead, bool (DatumCell::*equalityCheck)(const DatumCell*) const)
	{
		const DatumCell *datum = listHead;

		// Do this in a single pass for efficiency
		// ProperList doesn't give us much here
		while(auto pair = datum_cast<PairCell>(datum))
		{
			if ((pair->car()->*equalityCheck)(obj))
			{
				return pair;
			}

			datum = pair->cdr();
		}

		if (datum == EmptyListCell::instance())
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

PairCell *lliby_cons(World &world, DatumCell *car, DatumCell *cdr)
{
	// Root the car and cdr for the next allocation
	alloc::StrongRef<DatumCell> carRef(world, car);
	alloc::StrongRef<DatumCell> cdrRef(world, cdr);
	
	// Explicitly allocate first so there's no ambiguity about what order the
	// allocation and reference updates are done
	alloc::RangeAlloc allocation(alloc::allocateRange(world, 1));

	return new (*allocation.begin()) PairCell(carRef, {cdrRef});
}

DatumCell *lliby_car(PairCell *pair)
{
	return pair->car();
}

DatumCell *lliby_cdr(PairCell *pair)
{
	return pair->cdr();
}

void lliby_set_car(PairCell *pair, DatumCell *obj)
{
	return pair->setCar(obj);
}

void lliby_set_cdr(PairCell *pair, DatumCell *obj)
{
	return pair->setCdr(obj);
}

std::uint32_t lliby_length(World &world, ListElementCell *head) 
{
	ProperList<DatumCell> properList(head);

	if (!properList.isValid())
	{
		signalError(world, "Non-list passed to list-length", {head});
	}

	return properList.length();
}

ListElementCell* lliby_make_list(World &world, std::uint32_t count, DatumCell *fill)
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

DatumCell* lliby_list_copy(World &world, DatumCell *sourceHead)
{
	// Find the number of pairs in the list
	// We can't use ProperList because we need to work with improper lists and non-list objects
	std::uint32_t pairCount = 0;

	for(auto pair = datum_cast<PairCell>(sourceHead);
		pair != nullptr;
		pair = datum_cast<PairCell>(pair->cdr()))
	{
		pairCount++;
	}

	if (pairCount == 0)
	{
		return sourceHead;
	}

	// Make sure we take a reference to this across the next allocation in case the GC runs
	alloc::StrongRef<DatumCell> sourceHeadRef(world, sourceHead);	

	auto destHead = static_cast<PairCell*>(alloc::allocateCells(pairCount));
	PairCell *destPair = destHead;

	// We've counted our pairs so this has to be a pair
	auto sourcePair = datum_unchecked_cast<const PairCell>(sourceHeadRef.data());

	// This is predecrement because the last pair is handled specially below this loop
	while(--pairCount)
	{
		// Create the new pair cdr'ed to the next pair
		new (destPair) PairCell(sourcePair->car(), destPair + 1);

		destPair++;

		// Move to the next pair
		sourcePair = datum_unchecked_cast<PairCell>(sourcePair->cdr());
	}
	
	// Place our last pair cdr'ed to the last cdr
	// For proper lists this is the empty list
	// For improper list this is another type of non-pair datum
	new (destPair) PairCell(sourcePair->car(), sourcePair->cdr());

	// All done!
	return destHead;
}

ListElementCell* lliby_list(ListElementCell *head)
{
	// Our calling convention requires that any rest parameters are passed as
	// a proper list. Because (list) is defined as only having rest args the
	// codegen will do the heavy lifting of building the list and we only have
	// to return it.
	return head;
}

DatumCell* lliby_append(World &world, ListElementCell *argHead)
{
	ProperList<DatumCell> argList(argHead);

	if (!argList.isValid())
	{
		signalError(world, "Invalid argument list passed to (append)", {argHead});
	}

	auto argCount = argList.length();

	if (argCount == 0)
	{
		// Nothing to append
		return EmptyListCell::instance();
	}

	// XXX: This is not very efficient
	std::vector<DatumCell*> appenedElements;
	size_t appendIndex = 0;

	auto argIt = argList.begin();

	while(--argCount)
	{
		auto argDatum = *(argIt++);
		auto listHead = datum_cast<ListElementCell>(argDatum);

		if (listHead == nullptr)
		{
			signalError(world, "Non-list passed to (append) in non-terminal position", {argDatum});
		}

		// Get the passed list
		ProperList<DatumCell> properList(listHead);
	
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

const DatumCell* lliby_memv(World &world, const DatumCell *obj, ListElementCell *listHead)
{
	return list_search(world, obj, listHead, &DatumCell::isEqv);
}

const DatumCell* lliby_member(World &world, const DatumCell *obj, ListElementCell *listHead)
{
	return list_search(world, obj, listHead, &DatumCell::isEqual);
}

}
