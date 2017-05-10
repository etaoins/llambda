#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/ProperList.h"
#include "binding/IntegerCell.h"

#include "core/init.h"
#include "core/World.h"

#include "assertions.h"
#include "stubdefinitions.h"

namespace
{

using namespace lliby;

void testAll(World &world)
{
	StringCell *valueA = StringCell::fromUtf8StdString(world, "A");
	StringCell *valueB = StringCell::fromUtf8StdString(world, "B");
	StringCell *valueC = StringCell::fromUtf8StdString(world, "C");

	{
		ProperList<AnyCell> *properList = ProperList<AnyCell>::create(world, {});

		ASSERT_TRUE(properList->begin() == properList->end());
		ASSERT_EQUAL(properList->size(), 0);
		ASSERT_TRUE(properList->empty());
	}

	{
		ProperList<AnyCell> *properList = ProperList<AnyCell>::create(world, {valueA, valueB, valueC});

		ASSERT_TRUE(properList->begin() != properList->end());
		ASSERT_EQUAL(properList->size(), 3);
		ASSERT_FALSE(properList->empty());

		auto it = properList->begin();
		// Be tricky with the increment operators
		ASSERT_EQUAL(*(it++), valueA);

		ASSERT_EQUAL(*it, valueB);

		ASSERT_EQUAL(*(++it), valueC);

		it++;
		ASSERT_TRUE(it == properList->end());
	}

	{
		ProperList<IntegerCell> *properList = ProperList<IntegerCell>::emplaceValues(world, {1, 2, 3});

		ASSERT_TRUE(properList->begin() != properList->end());
		ASSERT_EQUAL(properList->size(), 3);
		ASSERT_FALSE(properList->empty());

		auto it = properList->begin();
		ASSERT_EQUAL((*(it++))->value(), 1);

		ASSERT_EQUAL((*it)->value(), 2);

		ASSERT_EQUAL((*(++it))->value(), 3);

		it++;
		ASSERT_TRUE(it == properList->end());
	}

	{
		AnyCell *improperList = ListElementCell::createList(world, {valueA, valueB}, valueC);

		auto stringImproperHead = cell_cast<ListElementCell>(improperList);
		ASSERT_TRUE(stringImproperHead != nullptr);

		auto properList = cell_cast<ProperList<AnyCell>>(stringImproperHead);

		ASSERT_NULL(properList);
	}
}

}

int main(int argc, char *argv[])
{
	llcore_run(testAll, argc, argv);
}
