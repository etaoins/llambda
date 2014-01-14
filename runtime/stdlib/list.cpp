#include "binding/PairCell.h"
#include "binding/EmptyListCell.h"
#include "binding/BooleanCell.h"
#include "binding/ProperList.h"
#include "core/fatal.h"
#include "alloc/allocator.h"
#include "alloc/RangeAlloc.h"

using namespace lliby;

namespace 
{
	// This is used to implement memq, memv and member without a callback
	const DatumCell* list_search(const DatumCell *obj, const ListElementCell *listHead, bool (DatumCell::*equalityCheck)(const DatumCell*) const)
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
			_lliby_fatal("Attempted to search non-list", listHead);
		}
	}
}

extern "C"
{

PairCell *lliby_cons(DatumCell *car, DatumCell *cdr)
{
	return new PairCell(car, cdr);
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

std::uint32_t lliby_length(const ListElementCell *head) 
{
	ProperList<DatumCell> properList(head);

	if (!properList.isValid())
	{
		_lliby_fatal("Non-list passed to list-length", head);
	}

	return properList.length();
}

ListElementCell* lliby_make_list(std::uint32_t count, DatumCell *fill)
{
	ListElementCell *cdr = const_cast<EmptyListCell*>(EmptyListCell::instance());

	// Allocate all the new pairs at once
	alloc::RangeAlloc allocation(alloc::allocateRange(count));
	auto allocIt = allocation.end();

	while(allocIt != allocation.begin())
	{
		cdr = new (*--allocIt) PairCell(fill, cdr);
	}

	return cdr;
}

ListElementCell* lliby_list_copy(const ListElementCell *sourceHead)
{
	// Find the number of pairs in the list
	// We can't use ProperList because we need to work with improper lists
	std::uint32_t pairCount = 0;

	for(auto pair = datum_cast<PairCell>(sourceHead);
		pair != nullptr;
		pair = datum_cast<PairCell>(pair->cdr()))
	{
		pairCount++;
	}

	if (pairCount == 0)
	{
		return const_cast<EmptyListCell*>(EmptyListCell::instance());
	}

	PairCell *destHead = static_cast<PairCell*>(alloc::allocateCells(pairCount));
	PairCell *destPair = destHead;

	// Because we're a proper list this has to be a pair
	const PairCell *sourcePair = static_cast<const PairCell*>(sourceHead);

	while(true)
	{
		DatumCell *sourceCdr = sourcePair->cdr();

		if (PairCell::isInstance(sourceCdr))
		{
			// Create the new pair cdr'ed to the next pair
			new (destPair) PairCell(sourcePair->car(), destPair + 1);

			destPair++;

			// Move to the next pair
			sourcePair = static_cast<PairCell*>(sourceCdr);
		}
		else
		{
			// Place our last pair cdr'ed to the last cdr
			// For proper lists this is the empty list
			// For improper list this is another type of non-pair datum
			new (destPair) PairCell(sourcePair->car(), sourceCdr);

			// All done!
			return destHead;
		}
	}
}

ListElementCell* lliby_list(ListElementCell *head)
{
	// Our calling convention requires that any rest parameters are passed as
	// a proper list. Because (list) is defined as only having rest args the
	// codegen will do the heavy lifting of building the list and we only have
	// to return it.
	return head;
}

DatumCell* lliby_append(ListElementCell *argHead)
{
	ProperList<DatumCell> argList(argHead);

	if (!argList.isValid())
	{
		_lliby_fatal("Invalid argument list passed to (append)", argHead);
	}

	auto argCount = argList.length();

	if (argCount == 0)
	{
		// Nothing to append
		return const_cast<EmptyListCell*>(EmptyListCell::instance());
	}

	// XXX: This is not very efficient
	std::list<DatumCell*> appenedElements;
	auto argIt = argList.begin();

	while(--argCount)
	{
		auto listHead = datum_cast<ListElementCell>(*(argIt++));

		if (listHead == nullptr)
		{
			_lliby_fatal("Non-list passed to (append) in non-terminal position", listHead);
		}

		// Get the passed list
		ProperList<DatumCell> properList(listHead);

		if (!properList.isValid())
		{
			_lliby_fatal("Improper list passed to (append) in non-terminal position", listHead);
		}

		for(auto element : properList)
		{
			appenedElements.push_back(element);
		}
	}

	// Use createList to append the last list on sharing its structure.
	// This is required by R7RS
	return ListElementCell::createList(appenedElements, *(argIt++));
}

const DatumCell* lliby_memv(const DatumCell *obj, const ListElementCell *listHead)
{
	return list_search(obj, listHead, &DatumCell::isEqv);
}

const DatumCell* lliby_member(const DatumCell *obj, const ListElementCell *listHead)
{
	return list_search(obj, listHead, &DatumCell::isEqual);
}

}
