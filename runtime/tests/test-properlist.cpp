#include "binding/StringCell.h"
#include "binding/SymbolCell.h"
#include "binding/ProperList.h"

#include "core/init.h"
#include "core/World.h"

#include "assertions.h"
#include "stubdefinitions.h"

#include "alloc/cellref.h"

namespace
{

using namespace lliby;

void testAll(World &world)
{
	alloc::StringRef valueA(world, StringCell::fromUtf8StdString(world, "A"));
	alloc::StringRef valueB(world, StringCell::fromUtf8StdString(world, "B"));
	alloc::StringRef valueC(world, StringCell::fromUtf8StdString(world, "C"));

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
		ASSERT_EQUAL(*(it++), valueA.data());

		ASSERT_EQUAL(*it, valueB.data());

		ASSERT_EQUAL(*(++it), valueC.data());

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
	lliby_init();

	lliby::World::launchWorld(&testAll);
}
