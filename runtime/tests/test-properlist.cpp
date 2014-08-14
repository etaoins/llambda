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
	alloc::StringRef valueA(world, StringCell::fromUtf8CString(world, "A"));
	alloc::StringRef valueB(world, StringCell::fromUtf8CString(world, "B"));
	alloc::StringRef valueC(world, StringCell::fromUtf8CString(world, "C"));
	
	{
		ListElementCell *emptyListHead = ListElementCell::createProperList(world, {});

		ProperList<AnyCell> properList(emptyListHead);

		ASSERT_TRUE(properList.isValid());
		ASSERT_TRUE(properList.begin() == properList.end());
		ASSERT_EQUAL(properList.length(), 0);
		ASSERT_TRUE(properList.isEmpty());
	}
	
	{
		ListElementCell *stringListHead = ListElementCell::createProperList(world, {valueA, valueB, valueC});

		ProperList<AnyCell> properList(stringListHead);

		ASSERT_TRUE(properList.isValid());
		ASSERT_TRUE(properList.begin() != properList.end());
		ASSERT_EQUAL(properList.length(), 3);
		ASSERT_FALSE(properList.isEmpty());

		auto it = properList.begin();
		// Be tricky with the increment operators
		ASSERT_EQUAL(*(it++), valueA.data());

		ASSERT_EQUAL(*it, valueB.data());
		
		ASSERT_EQUAL(*(++it), valueC.data());

		it++;
		ASSERT_TRUE(it == properList.end());
	}
	
	{
		AnyCell *improperList = ListElementCell::createList(world, {valueA, valueB}, valueC);
		
		auto stringImproperHead = cell_cast<ListElementCell>(improperList);
		ASSERT_TRUE(stringImproperHead != nullptr);

		// Improper list
		ProperList<StringCell> properList(stringImproperHead);

		ASSERT_FALSE(properList.isValid());
		ASSERT_TRUE(properList.begin() == properList.end());
		ASSERT_EQUAL(properList.length(), 0);
		ASSERT_TRUE(properList.isEmpty());
	}
}

}

int main(int argc, char *argv[])
{
	lliby_init();

	lliby::World::launchWorld(&testAll);
}
